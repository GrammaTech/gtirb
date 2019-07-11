from uuid import UUID, uuid4

import ImageByteMap_pb2

from gtirb.bytemap import ByteMap


class ImageByteMap:
    """
    Contains the loaded raw image data for the module (binary).
    """

    def __init__(self,
                 uuid=None,
                 byte_map=None,
                 addr_min=0,
                 addr_max=0,
                 base_address=0,
                 entry_point_address=0,
                 uuid_cache=None):
        if uuid is None:
            uuid = uuid4()
        if byte_map is None:
            byte_map = ByteMap()
        self.uuid = uuid
        if uuid_cache is not None:
            uuid_cache[uuid] = self
        self.byte_map = byte_map
        self.addr_min = addr_min
        self.addr_max = addr_max
        self.base_address = base_address
        self.entry_point_address = entry_point_address

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = ImageByteMap_pb2.ImageByteMap()
        ret.uuid = self.uuid.bytes
        ret.byte_map.CopyFrom(self.byte_map._to_protobuf())
        ret.addr_min = self.addr_min
        ret.addr_max = self.addr_max
        ret.base_address = self.base_address
        ret.entry_point_address = self.entry_point_address
        return ret

    @classmethod
    def _from_protobuf(cls, image_byte_map, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=image_byte_map.uuid)
        if uuid_cache is not None and uuid in uuid_cache:
            return uuid_cache[uuid]
        image_byte_map = cls(
            uuid=uuid,
            byte_map=ByteMap._from_protobuf(image_byte_map.byte_map),
            addr_min=image_byte_map.addr_min,
            addr_max=image_byte_map.addr_max,
            base_address=image_byte_map.base_address,
            entry_point_address=image_byte_map.entry_point_address,
            uuid_cache=uuid_cache)
        return image_byte_map
