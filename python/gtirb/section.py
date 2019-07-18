import Section_pb2

from gtirb.node import Node


class Section(Node):
    """
    Represents a named section of the binary.

    Does not directly store the contents of the section, which are
    kept in ImageByteMap.
    """

    def __init__(self, name='', address=0, size=0, uuid=None):
        super().__init__(uuid)
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
    def _decode_protobuf(cls, section, uuid):
        return cls(section.name, section.address, section.size, uuid)
