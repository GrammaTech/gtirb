from uuid import UUID

from .proto import Offset_pb2


class Offset:
    """
    An Offset describes a location inside a :class:`gtirb.CodeBlock`
    or :class:`gtirb.DataBlock`.

    :ivar ~.element_id: The UUID of a :class:`gtirb.ByteBlock`
            containing the location of interest.
    :ivar ~.displacement: The offset inside the Node to point to.
    """

    def __init__(self, element_id, displacement):
        # type: (UUID,int) -> None
        """
        :param element_id: The UUID of a :class:`gtirb.ByteBlock`
            containing the location of interest.
        :param displacement: The offset inside the Node to point to.
        """

        self.element_id = element_id  # type: UUID
        self.displacement = displacement  # type: int

    @classmethod
    def _from_protobuf(cls, offset):
        # type: (Offset_pb2.Offset) -> Offset
        """Decode a Protobuf object to an offset.

        :param offset: The Protobuf object.
        """

        return cls(UUID(bytes=offset.element_id), offset.displacement)

    def _to_protobuf(self):
        # type: () -> Offset_pb2.Offset
        """Encode this offset into a Protobuf object."""

        proto_offset = Offset_pb2.Offset()
        proto_offset.element_id = self.element_id.bytes
        proto_offset.displacement = self.displacement
        return proto_offset

    def __repr__(self):
        # type: () -> str
        return (
            "Offset("
            "element_id={element_id!r}, "
            "displacement={displacement!r}, "
            ")".format(**self.__dict__)
        )
