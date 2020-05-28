from typing import NamedTuple
from uuid import UUID

from .node import Node
from .proto import Offset_pb2


class Offset(
    NamedTuple("NamedTuple", (("element_id", Node), ("displacement", int)))
):
    """
    An Offset describes a location inside a :class:`gtirb.Node`, such as a
    :class:`gtirb.DataBlock` or :class:`gtirb.ByteInterval`.

    :ivar ~.element_id: The :class:`gtirb.Node` containing the location of
            interest.
    :ivar ~.displacement: The offset inside the Node to point to.
    """

    @classmethod
    def _from_protobuf(cls, offset, ir):
        # type: (Offset_pb2.Offset) -> Offset
        """Decode a Protobuf object to an offset.

        :param offset: The Protobuf object.
        """

        element_id = UUID(bytes=offset.element_id)
        return cls(ir.get_by_uuid(element_id), offset.displacement)

    def _to_protobuf(self):
        # type: () -> Offset_pb2.Offset
        """Encode this offset into a Protobuf object."""

        proto_offset = Offset_pb2.Offset()
        proto_offset.element_id = self.element_id.uuid.bytes
        proto_offset.displacement = self.displacement
        return proto_offset
