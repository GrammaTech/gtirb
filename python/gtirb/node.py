from uuid import UUID, uuid4
from weakref import WeakValueDictionary


class Node:
    """Base class for 'nodes', which can be referenced by their UUID"""
    _uuid_cache = WeakValueDictionary()

    def __init__(self, uuid=None):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid

    @classmethod
    def _decode_protobuf(cls, proto_object, uuid):
        raise NotImplementedError

    @classmethod
    def _from_protobuf(cls, proto_object):
        """The default implementation of _from_protobuf performs a cache lookup
        for the object's UUID in the cache, calling the appropriate
        _decode_protobuf constructor if cannot find it.
        """
        uuid = UUID(bytes=proto_object.uuid)
        if uuid in Node._uuid_cache:
            return Node._uuid_cache[uuid]
        new_node = cls._decode_protobuf(proto_object, uuid)
        Node._uuid_cache[new_node.uuid] = new_node
        return new_node
