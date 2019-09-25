import Section_pb2

from .node import Node


class Section(Node):
    """Represents a named section of the binary.

    Does not directly store the contents of the section, which are
    kept in ImageByteMap.

    :ivar name: the name of this section
    :ivar address: the address this section is located at in memory
    :ivar size: the size of this section, in bytes
    """

    def __init__(self, name='', address=0, size=0, uuid=None):
        """
        :param name: the value of :attr:`self.name`
        :param address: the value of :attr:`self.address`
        :param size: the value of :attr:`self.size`
        :param uuid: as in :meth:`gtirb.Node.__init__`
        """

        super().__init__(uuid)
        self.address = address
        self.name = name
        self.size = size

    @classmethod
    def _decode_protobuf(cls, section, uuid):
        return cls(section.name, section.address, section.size, uuid)

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        proto_section = Section_pb2.Section()
        proto_section.uuid = self.uuid.bytes
        proto_section.name = self.name
        proto_section.address = self.address
        proto_section.size = self.size
        return proto_section

    def deep_eq(self, other):
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, Section):
            return False
        return (
            self.uuid == other.uuid
            and self.address == other.address
            and self.name == other.name
            and self.size == other.size
        )

    def __repr__(self):
        return (
            "Section("
            "uuid={uuid!r}, "
            "name={name!r}, "
            "address={address:#x}, "
            "size={size!r}, "
            ")".format(**self.__dict__)
        )
