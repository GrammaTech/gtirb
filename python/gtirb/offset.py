from typing import TYPE_CHECKING, NamedTuple, Optional, Union
from uuid import UUID

from .node import Node
from .proto import Offset_pb2
from .util import DeserializationError

if TYPE_CHECKING:  # pragma: no cover
    # Ignore flake8 "imported but unused" errors.
    from .ir import IR  # noqa: F401


class Offset(
    NamedTuple(
        "NamedTuple",
        (("element_id", Union[UUID, Node]), ("displacement", int)),
    )
):
    """
    An Offset describes a location inside a :class:`gtirb.Node`, such as a
    :class:`gtirb.DataBlock` or :class:`gtirb.ByteInterval`.

    :ivar ~.element_id: The :class:`gtirb.Node` containing the location of
            interest.
    :ivar ~.displacement: The offset inside the Node to point to.
    """

    @classmethod
    def _from_protobuf(
        cls, offset: Offset_pb2.Offset, ir: Optional["IR"]
    ) -> "Offset":
        """Decode a Protobuf object to an offset.

        :param offset: The Protobuf object.
        """

        assert ir
        element_id = UUID(bytes=offset.element_id)
        element = ir.get_by_uuid(element_id)
        if not element:
            raise DeserializationError(
                "Offset: UUID %s does not refer to a Node" % element_id
            )
        return cls(element, offset.displacement)

    def _to_protobuf(self) -> Offset_pb2.Offset:
        """Encode this offset into a Protobuf object."""

        proto_offset = Offset_pb2.Offset()
        if isinstance(self.element_id, UUID):
            proto_offset.element_id = self.element_id.bytes
        else:
            proto_offset.element_id = self.element_id.uuid.bytes
        proto_offset.displacement = self.displacement
        return proto_offset
