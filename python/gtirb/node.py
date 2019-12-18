from uuid import UUID, uuid4
from weakref import WeakValueDictionary
import typing


class Node:
    """A Node is any GTIRB object which can be referenced by UUID.

    :ivar uuid: The UUID of this Node.
    """

    _uuid_cache = (
        WeakValueDictionary()
    )  # type: typing.ClassVar[typing.Mapping[UUID, "Node"]]

    def __init__(self, uuid=None):
        # type: (typing.Optional[UUID]) -> None
        """
        :param uuid: The UUID of this ``Node``,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        """

        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid  # type: UUID
        Node._uuid_cache[self.uuid] = self

    @classmethod
    def from_uuid(cls, uuid):
        # type: (UUID) -> typing.Optional[Node]
        """
        Get the ``Node`` with the specified UUID,
        or ``None`` if no such ``Node`` exists.

        :param uuid: The UUID to look up.
        :raises TypeError: if the Node is not of the requested type.
            To request that the output be of a specific type, call this
            function from the desired subclass. For example, calling
            ``gtirb.Block.from_uuid`` will return a :class:`gtirb.Block` or
            raise a ``TypeError``.
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
        # type: (typing.Any, UUID) -> Node
        """Decode a Protobuf object to a Python GTIRB object.
        Must be overridden by subclasses.

        :param proto_object: The Protobuf object.
        :param uuid: The UUID of the object.
        """

        raise NotImplementedError

    @classmethod
    def _from_protobuf(cls, proto_object):
        # type: (typing.Any) -> Node
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
        # type: () -> typing.Any
        """Get a Protobuf representation of ``self``.
        Must be overridden by subclasses.
        """

        raise NotImplementedError

    def deep_eq(self, other):
        # type: (typing.Any) -> bool
        """Check: is ``self`` structurally equal to ``other``?

        This method should be used only when deep structural equality checks
        are actually needed, and not for all equality checks. Typically the
        default implmentation of __eq__, which checks pointer equality, is
        sufficient; Nodes are cached such that references to two Nodes with
        the same UUID refer to the same exact object. Use this method when
        you have manually constructed Nodes that may share the same UUID
        despite being different objects, and you need to check for structural
        equality.
        """

        raise NotImplementedError
