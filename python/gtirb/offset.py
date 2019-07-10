import Offset_pb2


class Offset:
    """
    Describes the location inside a block or data object.
    """

    def __init__(self, element_id, offset):
        self.element_id = element_id
        self.offset = offset

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = Offset_pb2.Offset()
        ret.element_id = self.element_id
        ret.offset = self.offset
        return ret

    @classmethod
    def _from_protobuf(cls, offset, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        return cls(offset.element_id, offset.offset)
