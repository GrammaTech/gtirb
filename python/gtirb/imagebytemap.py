from bisect import bisect_right
from itertools import groupby
from operator import itemgetter
from uuid import UUID, uuid4

import ByteMap_pb2
import ImageByteMap_pb2


class ImageByteMap:
    """
    Contains the loaded raw image data for the module (binary).
    """

    def __init__(self,
                 *,
                 addr_min=0,
                 addr_max=0,
                 base_address=0,
                 byte_map=dict(),
                 entry_point_address=0,
                 uuid=None,
                 uuid_cache):
        self.addr_min = addr_min
        self.addr_max = addr_max
        self.base_address = base_address
        self.entry_point_address = entry_point_address
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
        uuid_cache[uuid] = self

        # Deep copy of the byte map
        self._byte_map = {k: bytearray(v) for k, v in byte_map.items()}
        self._start_addresses = sorted(self._byte_map.keys())

        # Validate byte_map by checking for overlapping regions while
        # identifying adjacent regions that can be combined
        max_addrs = [addr + len(self._byte_map[addr])
                     for addr in self._start_addresses]
        to_combine = set()
        for i, (max_addr, next_addr) in \
                enumerate(zip(max_addrs[:-1], self._start_addresses[1:])):
            if max_addr > next_addr:
                raise ValueError("address ranges in byte map overlap")
            if max_addr == next_addr:
                to_combine |= {i, i + 1}

        # Combine adjacent regions. Code for grouping consecutive numbers
        # via https://stackoverflow.com/a/2361991
        combine_indices = enumerate(sorted(to_combine))
        for _, g in groupby(combine_indices, lambda ix: ix[0] - ix[1]):
            start_index, *rest = map(itemgetter(1), g)
            start_addr = self._start_addresses[start_index]
            new_range = self._byte_map[start_addr]
            for next_index in rest:
                next_addr = self._start_addresses[next_index]
                del self._start_addresses[next_index]
                new_range += self._byte_map[next_addr]
                del self._byte_map[next_addr]
            self._byte_map[start_addr] = new_range

    def _find_start(self, address):
        """Find the greatest start less than or equal to address"""
        i = bisect_right(self._start_addresses, address)
        if i:
            return self._start_addresses[i-1]
        raise IndexError

    def __getitem__(self, key):
        """Random access of a single byte returns a byte if it exists, raises
        an `IndexError` if the byte does not exist at the address.
        Range access works as follows:
            [address:] -> get bytes starting at `address` and ending at the
                the last byte in the range containing `address`
            [address:end] -> get bytes between `address` and `end` if there are
                no gaps. Raise an `IndexError` otherwise.
            [:end] -> `IndexError`, start address is required
            [address:end:step_size] -> `IndexError`. Step size is unsupported.
        """
        def check_range(address):
            if address < self.addr_min or address > self.addr_max:
                raise IndexError

        # Single byte access
        if isinstance(key, int):
            check_range(key)
            start_address = self._find_start(key)
            region = self._byte_map[start_address]
            offset = key - start_address
            return region[offset]

        # Range access
        if isinstance(key, slice):
            if key.start is None:
                raise IndexError("start address required")
            if key.step is not None:
                raise IndexError("step size unsupported")
            check_range(key.start)
            start_address = self._find_start(key.start)
            region = self._byte_map[start_address]
            start_offset = key.start - start_address
            if key.stop is None:
                # No stop address
                return region[start_offset:]

            check_range(key.stop)
            if key.start > key.stop:
                raise IndexError("reverse slicing unsupported")
            stop_address = self._find_start(key.stop - 1)
            if start_address != stop_address:
                raise IndexError("gap in bytes between start and stop")
            stop_offset = key.stop - start_address
            return region[start_offset:stop_offset]

    def __setitem__(self, address, data):
        """Sets data starting at `address` to `data`.
        `data` can be a single byte passed as an integer in range(256) or
        several bytes passed as literal bytes, a bytearray, or an iterable
        of integers in range(256).

        If `data` is longer than one byte, each byte will be copied starting
        at `address` and overwriting adjacent bytes.
        It is an exception to try to write beyond the end of a region.
        """
        if isinstance(address, slice):
            if address.start is None:
                raise ValueError("must provide start address")
            address = address.start
        if isinstance(data, int):
            try:
                data = data.to_bytes(1, byteorder='big')
            except OverflowError:
                raise ValueError("could not interpret %d as a byte" % (data))
        else:
            data = bytearray(data)
        if address < self.addr_min or address > self.addr_max:
            raise IndexError
        start_address = self._find_start(address)
        available_space = len(self.__getitem__(slice(address, None, None)))
        if len(data) > available_space:
            raise ValueError("not enough space for %s" % (data))
        region = self._byte_map[start_address]
        offset = address - start_address
        for new_byte in data:
            region[offset] = new_byte
            offset += 1

    def __len__(self):
        return sum(len(v) for v in self._byte_map.values())

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        byte_map = ByteMap_pb2.ByteMap()

        def encode_region(region):
            proto_region = ByteMap_pb2.Region()
            proto_region.address, proto_region.data = region
            return proto_region
        byte_map.regions.extend(encode_region(r) for r in self.regions)

        image_byte_map = ImageByteMap_pb2.ImageByteMap()
        image_byte_map.addr_min = self.addr_min
        image_byte_map.addr_max = self.addr_max
        image_byte_map.base_address = self.base_address
        image_byte_map.byte_map.CopyFrom(byte_map)
        image_byte_map.entry_point_address = self.entry_point_address
        image_byte_map.uuid = self.uuid.bytes
        return image_byte_map

    @classmethod
    def _from_protobuf(cls, proto_image_byte_map, uuid_cache):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=proto_image_byte_map.uuid)
        if uuid in uuid_cache:
            return uuid_cache[uuid]
        byte_map = {region.address: bytearray(region.data)
                    for region in proto_image_byte_map.byte_map.regions}
        image_byte_map = cls(
            addr_min=proto_image_byte_map.addr_min,
            addr_max=proto_image_byte_map.addr_max,
            base_address=proto_image_byte_map.base_address,
            byte_map=byte_map,
            entry_point_address=proto_image_byte_map.entry_point_address,
            uuid=uuid,
            uuid_cache=uuid_cache)
        uuid_cache[uuid] = image_byte_map
        return image_byte_map
