import Offset_pb2


class Offset:
    """
    Describes the location inside a block or data object.
    """

    def __init__(self, element_id, displacement):
        self.element_id = element_id
        self.displacement = displacement

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        offset = Offset_pb2.Offset()
        offset.element_id = self.element_id
        offset.displacement = self.displacement
        return offset

    @classmethod
    def _from_protobuf(cls, offset, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        return cls(offset.element_id, offset.displacement)
