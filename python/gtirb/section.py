import Section_pb2
import typing
from uuid import UUID

from .node import Node


class Section(Node):
    """Represents a named section of the binary.

    Does not directly store the contents of the section, which are
    kept in a :class:`gtirb.ImageByteMap`.

    :ivar name: The section name (E.g. ".text", ".bss", etc).
    :ivar address: The address this section begins at in memory.
    :ivar size: The size of this section in bytes.
    """

    def __init__(
        self,
        name="",  # type: str
        address=0,  # type: int
        size=0,  # type: int
        uuid=None,  # type: typing.Optional[UUID]
    ):
        """
        :param name: The name of this section.
        :param address: The address this section is located at in memory.
        :param size: The size of this section in bytes.
        :param uuid: The UUID of this ``Section``,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        """

        super().__init__(uuid)
        self.address = address  # type: int
        self.name = name  # type: str
        self.size = size  # type: int

    @classmethod
    def _decode_protobuf(cls, section, uuid):
        # type: (Section_pb2.Section, UUID) -> Section
        return cls(section.name, section.address, section.size, uuid)

    def _to_protobuf(self):
        # type: () -> Section_pb2.Section
        """Get a Protobuf representation of ``self``."""

        proto_section = Section_pb2.Section()
        proto_section.uuid = self.uuid.bytes
        proto_section.name = self.name
        proto_section.address = self.address
        proto_section.size = self.size
        return proto_section

    def deep_eq(self, other):
        # type: (typing.Any) -> bool
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
        # type: () -> str
        return (
            "Section("
            "uuid={uuid!r}, "
            "name={name!r}, "
            "address={address:#x}, "
            "size={size!r}, "
            ")".format(**self.__dict__)
        )
