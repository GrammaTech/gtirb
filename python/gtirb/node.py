from uuid import UUID, uuid4
from weakref import WeakValueDictionary


class Node:
    """A Node is any GTIRB object which can be referenced by UUID.

    :ivar uuid: The UUID of this Node.
    """

    _uuid_cache = WeakValueDictionary()

    def __init__(self, uuid=None):
        """
        :param uuid: The UUID of this Node,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        """

        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
        Node._uuid_cache[self.uuid] = self

    @classmethod
    def from_uuid(cls, uuid):
        """
        Find the ``Node`` with the specified UUID,
        or ``None`` if no such ``Node`` can be found.

        :param uuid: The UUID to look up.
        :raises TypeError: if the Node is not of the requested type.
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

        :param proto_object: The Protobuf object.
        :param uuid: The UUID of the object.
        """

        raise NotImplementedError

    @classmethod
    def _from_protobuf(cls, proto_object):
        """Deserialize a Node from Protobuf.

        Performs a cache lookup for the object's UUID in the cache, calling the
        class' _decode_protobuf constructor if cannot find it.
        """

        uuid = UUID(bytes=proto_object.uuid)
        node = cls.from_uuid(uuid)
        if node is None:
            node = cls._decode_protobuf(proto_object, uuid)
        return node

    def _to_protobuf(self):
        """Returns a Protobuf representation of the object.
        Must be overridden by subclasses.
        """

        raise NotImplementedError

    def deep_eq(self, other):
        """Compare structural equality of two Nodes.

        This method should be used only when deep structural equality checks
        are actually needed, and not for all equality checks. Typically the
        default implmentation of __eq__, which checks pointer equality, is
        enough: UUID checks are part of deep equality checks, and generating a
        new Node generates a unique UUID.
        """

        raise NotImplementedError
