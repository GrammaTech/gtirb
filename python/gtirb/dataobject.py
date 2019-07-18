import DataObject_pb2

from gtirb.node import Node


class DataObject(Node):
    """
    Represents a data object, possibly symbolic.

    Does not directly store the data bytes, which are kept in the
    ImageByteMap.
    """

    def __init__(self, address=0, size=0, uuid=None):
        super().__init__(uuid)
        self.address = address
        self.size = size

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = DataObject_pb2.DataObject()
        ret.uuid = self.uuid.bytes
        ret.address = self.address
        ret.size = self.size
        return ret

    @classmethod
    def _decode_protobuf(cls, proto_data, uuid):
        return cls(data_object.address, data_object.size, uuid)
