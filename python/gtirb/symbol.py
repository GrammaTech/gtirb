from enum import Enum
from uuid import UUID
import typing

import Symbol_pb2

from .block import Block
from .node import Node


Payload = typing.Union[Block, int]
"""A type hint representing the possible Symbol payloads."""


class Symbol(Node):
    """Represents a symbol, which maps a name to an object in the IR.

    :ivar name: The name of this symbol.
    :ivar storage_kind: The storage kind of this symbol.
    """

    class StorageKind(Enum):
        """
        The storage kind of a :class:`gtirb.Symbol`.
        """

        Undefined = Symbol_pb2.StorageKind.Value("Storage_Undefined")
        """An unspecified storage kind."""

        Normal = Symbol_pb2.StorageKind.Value("Storage_Normal")
        """The symbol is accessible outside the module."""

        Static = Symbol_pb2.StorageKind.Value("Storage_Static")
        """The symbol is accessible only within the module."""

        Extern = Symbol_pb2.StorageKind.Value("Storage_Extern")
        """The symbol is defined outside of this module."""

        Local = Symbol_pb2.StorageKind.Value("Storage_Local")
        """The symbol is stored locally,
        in the context of a function's activation frame.
        """

    def __init__(
        self,
        name,  # type: str
        storage_kind=StorageKind.Undefined,  # type: Symbol.StorageKind
        uuid=None,  # type: typing.Optional[UUID]
        payload=None,  # type: typing.Optional[Payload]
    ):
        """
        :param name: The name of this symbol.
        :param storage_kind: The storage kind of this symbol.
        :param uuid: The UUID of this ``Symbol``,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        :param payload: The value this symbol points to.
            May be an address, a Node, or None.
        """

        super().__init__(uuid)
        self.name = name  # type: str
        self.storage_kind = storage_kind  # type: Symbol.StorageKind
        self._payload = payload  # type: typing.Optional[Payload]

    @property
    def value(self):
        # type: () -> typing.Optional[int]
        """The value of a Symbol, which is an integer or None.
        ``value`` and ``referent`` are mutually exclusive.
        """

        if not isinstance(self._payload, Block):
            return self._payload
        return None

    @property
    def referent(self):
        # type: () -> typing.Optional[Block]
        """The object referred to by a Symbol, which is :class:`Block`
        or None. ``value`` and ``referent`` are mutually exclusive.
        """

        if isinstance(self._payload, Block):
            return self._payload
        return None

    @value.setter
    def value(self, value):
        # type: (typing.Optional[int]) -> None
        self._payload = value

    @referent.setter
    def referent(self, referent):
        # type: (typing.Optional[Block]) -> None
        self._payload = referent

    @classmethod
    def _decode_protobuf(cls, proto_symbol, uuid):
        # type: (Symbol_pb2.Symbol,UUID) -> Symbol
        storage_kind = Symbol.StorageKind(proto_symbol.storage_kind)
        symbol = cls(
            name=proto_symbol.name, uuid=uuid, storage_kind=storage_kind
        )
        if proto_symbol.HasField("value"):
            symbol.value = proto_symbol.value
        if proto_symbol.HasField("referent_uuid"):
            referent_uuid = UUID(bytes=proto_symbol.referent_uuid)
            try:
                symbol.referent = Node._uuid_cache[referent_uuid]
            except KeyError as e:
                raise KeyError("Could not find referent UUID %s" % e)
        return symbol

    def _to_protobuf(self):
        # type: () -> Symbol_pb2.Symbol
        proto_symbol = Symbol_pb2.Symbol()
        proto_symbol.uuid = self.uuid.bytes
        if self.value is not None:
            proto_symbol.value = self.value
        elif self.referent is not None:
            proto_symbol.referent_uuid = self.referent.uuid.bytes
        proto_symbol.name = self.name
        proto_symbol.storage_kind = self.storage_kind.value
        return proto_symbol

    def deep_eq(self, other):
        # type: (typing.Any) -> bool
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, Symbol):
            return False
        if self.value != other.value:
            return False
        elif (
            self.referent is not None
            and self.referent.uuid != other.referent.uuid
        ):
            return False
        return (
            self.name == other.name
            and self.storage_kind == other.storage_kind
            and self.uuid == other.uuid
        )

    def __repr__(self):
        # type: () -> str
        return (
            "Symbol("
            "uuid={uuid!r}, "
            "name={name!r}, "
            "storage_kind=Symbol.{storage_kind!s}, "
            "payload={_payload!r}, "
            ")".format(**self.__dict__)
        )
