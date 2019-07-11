from uuid import UUID, uuid4

import DataObject_pb2


class DataObject:
    """
    Represents a data object, possibly symbolic.

    Does not directly store the data bytes, which are kept in the
    ImageByteMap.
    """

    def __init__(self, uuid=None, address=0, size=0, uuid_cache=None):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
        if uuid_cache is not None:
            uuid_cache[uuid] = self
        self.address = address
        self.size = size

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = DataObject_pb2.DataObject()
        ret.uuid = self.uuid.bytes
        ret.address = self.address
        ret.size = self.size
        return ret

    @classmethod
    def _from_protobuf(cls, data_object, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=data_object.uuid)
        if uuid_cache is not None and uuid in uuid_cache:
            return uuid_cache[uuid]
        return cls(uuid, data_object.address, data_object.size, uuid_cache)
