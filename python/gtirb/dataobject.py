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

    @classmethod
    def _decode_protobuf(cls, proto_dataobject, uuid):
        return cls(proto_dataobject.address, proto_dataobject.size, uuid)

    def to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        proto_dataobject = DataObject_pb2.DataObject()
        proto_dataobject.uuid = self.uuid.bytes
        proto_dataobject.address = self.address
        proto_dataobject.size = self.size
        return proto_dataobject
