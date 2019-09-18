from uuid import UUID, uuid4
from weakref import WeakValueDictionary


class DecodeError(Exception):
    """Exception during decoding"""

    def __init__(self, msg):
        self.msg = msg


class Node:
    """Base class for 'nodes', which can be referenced by their UUID"""

    _uuid_cache = WeakValueDictionary()

    def __init__(self, uuid=None):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
        Node._uuid_cache[self.uuid] = self

    @classmethod
    def from_uuid(cls, uuid):
        """
        Find the Node the corresponds to a given UUID,
        or None if not found.
        """
        node = Node._uuid_cache.get(uuid)
        if node is not None and not isinstance(node, cls):
            raise TypeError(
                "%s is node of type %s, not %s"
                % (uuid, type(node).__name__, cls.__name__)
            )
        return node

    @classmethod
    def _decode_protobuf(cls, proto_object, uuid):
        """Decode a Protobuf object to a Python GTIRB object.

        Must be overridden by subclasses.

        Parameters:
            cls: the Python GTIRB class to construct
            proto_object: the Protobuf object
            uuid: the UUID of the object. Automatically inserted into the
                cache by _from_protobuf.

        """
        raise NotImplementedError

    @classmethod
    def _from_protobuf(cls, proto_object):
        """Deserialize a Node from Protobuf

        Performs a cache lookup for the object's UUID in the cache, calling the
        class' _decode_protobuf constructor if cannot find it.

        """
        uuid = UUID(bytes=proto_object.uuid)
        node = cls.from_uuid(uuid)
        if node is None:
            node = cls._decode_protobuf(proto_object, uuid)
        return node

    def _to_protobuf(self):
        """Return a Protobuf representation of the object.

        Must be overridden by subclasses.

        """
        raise NotImplementedError

    def deep_eq(self, other):
        """Compare structural equality

        This method should be used only when deep structural equality checks
        are actually needed, and not for all equality checks. Typically the
        default implmentation of __eq__, which checks pointer equality, is
        enough: UUID checks are part of deep equality checks, and generating a
        new Node generates a unique UUID.

        """
        raise NotImplementedError
