import DataObject_pb2

from .node import Node


class DataObject(Node):
    """Represents a data object, possibly symbolic.

    Does not directly store data bytes, which are kept in an ImageByteMap.

    :ivar address: the address of the data object
    :ivar size: the size of the data object
    """

    def __init__(self, address, size, uuid=None):
        """
        :param address: the value of :attr:`self.address`
        :param size: the value of :attr:`self.size`
        :param uuid: as in :meth:`gtirb.Node.__init__`
        """

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

    def deep_eq(self, other):
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, DataObject):
            return False
        return (
            self.uuid == other.uuid
            and self.address == other.address
            and self.size == other.size
        )

    def __repr__(self):
        return (
            "DataObject("
            "uuid={uuid!r}, "
            "address={address:#x}, "
            "size={size}, "
            ")".format(**self.__dict__)
        )
