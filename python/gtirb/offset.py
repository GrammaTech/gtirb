from uuid import UUID

import Offset_pb2


class Offset:
    """
    Describes the location inside a block or data object.
    """

    def __init__(self, element_id, displacement):
        self.element_id = element_id
        self.displacement = displacement

    @classmethod
    def _from_protobuf(cls, offset):
        """
        Load this cls from protobuf object
        """
        return cls(UUID(bytes=offset.element_id), offset.displacement)

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        proto_offset = Offset_pb2.Offset()
        proto_offset.element_id = self.element_id.bytes
        proto_offset.displacement = self.displacement
        return proto_offset

    def __repr__(self):
        return ("Offset("
                "element_id={element_id!r}, "
                "displacement={displacement!r}, "
                ")".format(**self.__dict__))
