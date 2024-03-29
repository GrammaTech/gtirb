import typing
from uuid import UUID, uuid4

import typing_extensions
from google.protobuf.message import Message

from .util import DeserializationError

if typing.TYPE_CHECKING:  # pragma: no cover
    # Ignore flake8 "imported but unused" errors.
    from .ir import IR  # noqa: F401


_T = typing.TypeVar("_T", bound="Node")


class _NodeMessage(typing_extensions.Protocol):
    uuid: bytes


class Node:
    """A Node is any GTIRB object which can be referenced by UUID.

    :ivar ~.uuid: The UUID of this Node.
    """

    def __init__(self, uuid: typing.Optional[UUID] = None) -> None:
        """
        :param uuid: The UUID of this ``Node``,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        """

        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid

    @classmethod
    def _decode_protobuf(
        cls: typing.Type[_T],
        proto_object: _NodeMessage,
        uuid: UUID,
        ir: typing.Optional["IR"],
    ) -> _T:
        """Decode a Protobuf object to a Python GTIRB object.
        Must be overridden by subclasses.

        :param proto_object: The Protobuf object.
        :param uuid: The UUID of the object.
        """

        raise NotImplementedError  # pragma: no cover

    @classmethod
    def _from_protobuf(
        cls: typing.Type[_T],
        proto_object: _NodeMessage,
        ir: typing.Optional["IR"],
    ) -> _T:
        """Deserialize a Node from Protobuf.

        Performs a cache lookup for the object's UUID in the cache, calling the
        class' _decode_protobuf constructor if cannot find it.
        """

        uuid = UUID(bytes=proto_object.uuid)
        node = None
        if ir is not None:
            cached_node = ir.get_by_uuid(uuid)
            if isinstance(cached_node, cls):
                node = cached_node
            elif cached_node is not None:
                raise DeserializationError(
                    "got %s for UUID %s but expected %s"
                    % (type(cached_node).__name__, uuid, cls.__name__)
                )
        if node is None:
            node = cls._decode_protobuf(proto_object, uuid, ir)
        return node

    def _to_protobuf(self) -> Message:
        """Get a Protobuf representation of ``self``.
        Must be overridden by subclasses.
        """

        raise NotImplementedError  # pragma: no cover

    def deep_eq(self, other: object) -> bool:
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

        raise NotImplementedError  # pragma: no cover
