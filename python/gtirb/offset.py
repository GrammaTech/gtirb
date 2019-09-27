from uuid import UUID

import Offset_pb2


class Offset:
    """
    An Offset describes a location inside a :class:`gtirb.Block`
    or :class:`gtirb.DataObject`.

    :ivar element_id: The UUID of a :class:`gtirb.Block`
        or :class:`gtirb.DataObject` containing the
        location of interest.
    :ivar displacement: The offset inside the Node to point to.
    """

    def __init__(self, element_id, displacement):
        # type: (UUID,int) -> None
        """
        :param element_id: The UUID of a :class:`gtirb.Block`
            or :class:`gtirb.DataObject` containing the
            location of interest.
        :param displacement: The offset inside the Node to point to.
        """

        self.element_id = element_id  # type: UUID
        self.displacement = displacement  # type: int

    @classmethod
    def _from_protobuf(cls, offset):
        # type: (Offset_pb2.Offset) -> Offset
        return cls(UUID(bytes=offset.element_id), offset.displacement)

    def _to_protobuf(self):
        # type: () -> Offset_pb2.Offset
        proto_offset = Offset_pb2.Offset()
        proto_offset.element_id = self.element_id.bytes
        proto_offset.displacement = self.displacement
        return proto_offset

    def __repr__(self):
        # type: () -> str
        return ("Offset("
                "element_id={element_id!r}, "
                "displacement={displacement!r}, "
                ")".format(**self.__dict__))
