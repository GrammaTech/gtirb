from uuid import UUID, uuid4

import Section_pb2


class Section:
    """
    Represents a named section of the binary.

    Does not directly store the contents of the section, which are
    kept in ImageByteMap.
    """

    def __init__(self, uuid=None, name='', address=0, size=0, uuid_cache=None):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
        if uuid_cache is not None:
            uuid_cache[uuid] = self
        self.name = name
        self.address = address
        self.size = size

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = Section_pb2.Section()
        ret.uuid = self.uuid.bytes
        ret.name = self.name
        ret.address = self.address
        ret.size = self.size
        return ret

    @classmethod
    def _from_protobuf(cls, section, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=section.uuid)
        if uuid_cache is not None and uuid in uuid_cache:
            return uuid_cache[uuid]
        return cls(uuid, section.name, section.address,
                   section.size, uuid_cache)
