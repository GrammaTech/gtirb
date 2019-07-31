import DataObject_pb2

from .node import Node


class DataObject(Node):
    """Represents a data object, possibly symbolic.

    Does not directly store data bytes, which are kept in an ImageByteMap.

    Attributes:
        address: the address of the data object
        size: the size of the data object
        uuid: the UUID of this Node

    """
    def __init__(self, address, size, uuid):
        super().__init__(uuid)
        self.address = address
        self.size = size

    @classmethod
    def _decode_protobuf(cls, proto_dataobject, uuid):
        return cls(proto_dataobject.address, proto_dataobject.size, uuid)

    def _to_protobuf(self):
        proto_dataobject = DataObject_pb2.DataObject()
        proto_dataobject.uuid = self.uuid.bytes
        proto_dataobject.address = self.address
        proto_dataobject.size = self.size
        return proto_dataobject
