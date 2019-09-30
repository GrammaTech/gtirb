import DataObject_pb2

from .node import Node


class DataObject(Node):
    """Represents a data object, possibly symbolic.

    Does not directly store data bytes, which are kept in an ImageByteMap.

    :ivar address: The address of the data object.
    :ivar size: The size of the data object in bytes.
    """

    def __init__(self, address, size, uuid=None):
        """
        :param address: The address of the data object.
        :param size: The size of the data object in bytes.
        :param uuid: The UUID of this Node,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
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
