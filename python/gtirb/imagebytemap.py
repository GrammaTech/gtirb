from bisect import bisect_right, bisect_left, insort

import ByteMap_pb2
import ImageByteMap_pb2

from .node import Node


class ImageByteMap(Node):
    """Contains the loaded raw image data for the module (binary).

    Allows dictionary-like access to and modification ofnbytes in the map
    through overridden __delitem__, __getitem__, and __setitem__ methods.

    Attributes:
        addr_min: the lowest address in the byte map
        addr_max: the highest address in the byte map
        base_address: the base address of the byte map
        entry_point_address: the entry point address of the byte map
        uuid: the UUID of this Node

    """
    def __init__(self,
                 *,
                 addr_min=0,
                 addr_max=0,
                 base_address=0,
                 byte_map=dict(),
                 entry_point_address=0,
                 uuid=None):
        """Create a new ImageByteMap

        Parameters:
            addr_min: the lowest address in the byte map
            addr_max: the highest address in the byte map
            base_address: the base address of the byte map
            byte_map: a dictionary holding a sparse mapping of addresses to
                data, stored in a bytearray
            entry_point_address: the entry point address of the byte map
            uuid: the UUID of this Node

        """
        super().__init__(uuid)
        self.addr_min = addr_min
        self.addr_max = addr_max
        self.base_address = base_address
        self.entry_point_address = entry_point_address

        # Deep copy of the byte map
        self._byte_map = {k: bytearray(v) for k, v in byte_map.items()}
        # Validate/coalesce address ranges
        self._start_addresses = list()
        addresses = sorted(byte_map.keys())
        if len(addresses) != 0 and addresses[0] < addr_min:
            raise ValueError("address in byte map out of range")
        max_addr = None
        for addr in addresses:
            if max_addr is None or max_addr < addr:
                self._start_addresses.append(addr)
                min_addr = addr
            elif max_addr == addr:
                self._byte_map[min_addr] += self._byte_map[addr]
                del self._byte_map[addr]
            else:
                raise ValueError("address ranges in byte map overlap")
            max_addr = min_addr + len(self._byte_map[min_addr])
        if max_addr is not None and max_addr - 1 > addr_max:
            raise ValueError("address in byte map out of range")

    def __contains__(self, key):
        """Checks if a single address is in the byte map"""
        if isinstance(key, int):
            if not self._in_range(key):
                return False
            try:
                self._find_end(key)
                return True
            except IndexError:
                return False
        return False

    def __delitem__(self, key):
        """Delete bytes in the map

        Takes an address or slice of addresses, raises an `IndexError` if the
        byte does not exist at the address. Slicing requires both a start and
        stop address.

        """
        # The only legal accesses are single indices or slices
        if not isinstance(key, (int, slice)):
            raise TypeError("index must be address or slice")

        # Change single indices into slices
        if isinstance(key, int):
            key = slice(key, key + 1, None)
        if not isinstance(key.start, int) or not isinstance(key.stop, int):
            raise TypeError("start and stop addresses must be integers")
        if key.start > key.stop:
            raise IndexError("reverse slicing unsupported")
        if key.step is not None:
            raise IndexError("step size unsupported")
        if key.start not in self:
            raise IndexError("start address not in map")
        if key.stop - 1 not in self:
            raise IndexError("stop address not in map")
        start_range_address = self._find_start(key.start)
        index = bisect_left(self._start_addresses, start_range_address)
        if start_range_address != self._find_start(key.stop):
            raise IndexError("gap in bytes in range")
        region = self._byte_map[start_range_address]
        start_offset = key.start - start_range_address
        stop_offset = key.stop - start_range_address

        # If the slice is the whole region, delete the whole region
        if len(region) == stop_offset - start_offset:
            del self._byte_map[start_range_address]
            del self._start_addresses[index]

        # If the slice stops at the last byte of the region, delete it
        elif stop_offset == len(region):
            del self._byte_map[start_range_address][start_offset:]

        # If the slice starts at the first byte of a region
        # there is no need to split
        elif start_offset == 0:
            new_region = region[stop_offset:]
            del self._byte_map[start_range_address]
            self._byte_map[key.stop] = new_region
            self._start_addresses[index] = key.stop

        # Otherwise split the region
        else:
            self._byte_map[start_range_address] = region[:start_offset]
            self._byte_map[key.stop] = region[stop_offset:]
            insort(self._start_addresses, key.stop)

    def __getitem__(self, key):
        """Access bytes in the map

        Random access of a single byte returns a byte if it exists, raises an
        `IndexError` if the byte does not exist at the address. Slicing
        requires both a start and stop address.

        """
        # Single byte access
        if isinstance(key, int):
            if key not in self:
                raise IndexError("no contents at %d" % key)
            start_address = self._find_start(key)
            region = self._byte_map[start_address]
            offset = key - start_address
            return region[offset]

        # Slice access
        elif isinstance(key, slice):
            if not isinstance(key.start, int) or not isinstance(key.stop, int):
                raise TypeError("start and stop addresses must be integers")
            if key.start == key.stop:
                return bytearray()
            if key.start > key.stop:
                raise IndexError("reverse slicing unsupported")
            if key.step is not None:
                raise IndexError("step size unsupported")
            if key.start not in self:
                raise IndexError("start address not in map")
            if key.stop-1 not in self:
                raise IndexError("stop address not in map")
            start_address = self._find_start(key.start)
            stop_address = self._find_start(key.stop)
            if start_address != stop_address:
                raise IndexError("gap in bytes in range")
            region = self._byte_map[start_address]
            start_offset = key.start - start_address
            stop_offset = key.stop - start_address
            return region[start_offset:stop_offset]

        # Other accesses are illegal
        raise TypeError("index must be address or slice")

    def __iter__(self):
        """Yields all bytes in all ranges in order

        Returns an (address, byte) tuple

        """
        for start_addr in self._start_addresses:
            cur_addr = start_addr
            for byte in self._byte_map[start_addr]:
                yield (cur_addr, byte)
                cur_addr += 1

    def __len__(self):
        """The number of bytes contained in the map"""
        return sum(len(v) for v in self._byte_map.values())

    def __setitem__(self, address, data):
        """Set data at an address

        If `address` is an integer, sets the byte at `address` to `data`.
        `data` must be a single byte passed in as an integer in range(256)

        If `address` is a slice, there are two options:
            - If the slice has a start and a stop (e.g., a[1:10]), `data` must
            be an iterable of bytes the same length as the slice. Otherwise a
            `ValueError` is raised.
            - If the slice has a start and no stop (e.g., a[1:]), `data` must
            be an iterable of bytes of any length. All bytes are written
            beginning at `address.start`

        It is an `IndexError` to try to write to bytes before addr_min or after
        addr_max. It is also an `IndexError` to provide a slice without a start
        or with a step.

        """
        # address is an integer
        if isinstance(address, int):
            if not self._in_range(address):
                raise IndexError("address out of range")

            # If a byte at this address exists, set it
            if address in self:
                start = self._find_start(address)
                offset = address - start
                array = self._byte_map[start]
                array[offset] = data
                return

            # Check for an existing array to add to
            array = None
            try:
                start_of_previous = self._find_start(address)
                end_of_previous = self._find_end(start_of_previous)
                if end_of_previous == address - 1:
                    array = self._byte_map[start_of_previous]
            except IndexError:
                pass

            # Add data
            if array is not None:
                array.append(data)
            else:
                new_array = bytearray(1)
                new_array[0] = data
                self._byte_map[address] = new_array
                insort(self._start_addresses, address)

            # Check if the next address is the start of a new array and
            # combine the two if needed
            previous_start = self._find_start(address)
            next_start = self._find_start(address + 1)
            if previous_start != next_start:
                previous_array = self._byte_map[previous_start]
                next_array = self._byte_map[next_start]
                previous_array += next_array
                del self._byte_map[next_start]
                old_index = bisect_left(self._start_addresses, next_start)
                del self._start_addresses[old_index]

        # address is a slice
        if isinstance(address, slice):
            if address.start is None:
                raise IndexError("slice requires a start address")
            if address.step is not None:
                raise IndexError("step size is not supported")
            if address.stop is not None:
                if address.start > address.stop:
                    raise IndexError("start is after stop")
                if address.stop - address.start != len(data):
                    raise ValueError("slice size and data length do not match")
            if address.start + len(data) - 1 > self.addr_max:
                raise IndexError("attempt to write past maximum address")
            # Write each byte one-at-a-time
            current_address = address.start
            for byte in data:
                try:
                    self[current_address] = byte
                except TypeError:
                    raise TypeError("data is not iterable of bytes")
                current_address += 1

    @classmethod
    def _decode_protobuf(cls, proto_ibm, uuid):
        byte_map = {region.address: bytearray(region.data)
                    for region in proto_ibm.byte_map.regions}
        image_byte_map = cls(
            addr_min=proto_ibm.addr_min,
            addr_max=proto_ibm.addr_max,
            base_address=proto_ibm.base_address,
            byte_map=byte_map,
            entry_point_address=proto_ibm.entry_point_address,
            uuid=uuid)
        return image_byte_map

    def _find_end(self, address):
        """Get the last address in the block containing address."""
        start = self._find_start(address)
        last_address = start + len(self._byte_map[start]) - 1
        if address > last_address:
            raise IndexError("no range containing %d" % (address))
        return last_address

    def _find_start(self, address):
        """Find the greatest start less than or equal to address"""
        i = bisect_right(self._start_addresses, address)
        if i:
            return self._start_addresses[i-1]
        raise IndexError("no range containing %d" % (address))

    def _in_range(self, key):
        """Check if a key is within the range of this bytemap"""
        return key >= self.addr_min and key <= self.addr_max

    def _to_protobuf(self):
        proto_byte_map = ByteMap_pb2.ByteMap()

        def encode_region(address, data):
            proto_region = ByteMap_pb2.Region()
            proto_region.address = address
            proto_region.data = bytes(data)
            return proto_region
        proto_byte_map.regions.extend(
            encode_region(address, data)
            for address, data in self._byte_map.items()
        )
        proto_ibm = ImageByteMap_pb2.ImageByteMap()
        proto_ibm.addr_min = self.addr_min
        proto_ibm.addr_max = self.addr_max
        proto_ibm.base_address = self.base_address
        proto_ibm.byte_map.CopyFrom(proto_byte_map)
        proto_ibm.entry_point_address = self.entry_point_address
        proto_ibm.uuid = self.uuid.bytes
        return proto_ibm

    def deep_eq(self, other):
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, ImageByteMap):
            return False
        if self.uuid != other.uuid \
                or self.addr_min != other.addr_min \
                or self.base_address != other.base_address \
                or self.entry_point_address != other.entry_point_address \
                or self._start_addresses != other._start_addresses:
            return False
        for addr in self._start_addresses:
            if self._byte_map[addr] != other._byte_map[addr]:
                return False
        return True

    def __repr__(self):
        return ("ImageByteMap("
                "uuid={uuid!r}, "
                "addr_min={addr_min:#x}, "
                "addr_max={addr_max:#x}, "
                "base_address={base_address:#x}, "
                "entry_point_address={entry_point_address:#x}, "
                "byte_map={_byte_map!r}, "
                ")".format(**self.__dict__))
