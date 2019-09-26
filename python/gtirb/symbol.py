from enum import Enum
from uuid import UUID
import typing

import Symbol_pb2

from .block import Block, ProxyBlock
from .dataobject import DataObject
from .node import Node


Referent = typing.Union[Block, ProxyBlock, DataObject]
"""A type hint representing the possible Symbol referents."""
Value = typing.Union[int]
"""A type hint representing the possible Symbol values."""
Payload = typing.Union[Referent, Value]
"""A type hint representing the possible Symbol payloads."""

class Symbol(Node):
    """Represents a symbol, which maps a name to an object in the IR.

    :ivar name: The name of this symbol.
    :ivar storage_kind: The storage kind of this symbol.
    """

    name: str
    storage_kind: "Symbol.StorageKind"
    _payload: typing.Optional[Payload]

    class StorageKind(Enum):
        """
        The storage kind of a :class:`gtirb.Symbol`.
        """

        Undefined = Symbol_pb2.StorageKind.Value('Storage_Undefined')
        """An unspecified storage kind."""

        Normal = Symbol_pb2.StorageKind.Value("Storage_Normal")
        """This symbol is accessible outside the module."""

        Static = Symbol_pb2.StorageKind.Value("Storage_Static")
        """This symbol is accessible only within the module."""

        Extern = Symbol_pb2.StorageKind.Value("Storage_Extern")
        """This symbol is defined outside of this module."""

        Local = Symbol_pb2.StorageKind.Value("Storage_Local")
        """This symbol is stored locally,
        in the context of a function's activation frame.
        """

    def __init__(self,
                 name: str,
                 storage_kind: "Symbol.StorageKind" = StorageKind.Undefined,
                 uuid: typing.Optional[UUID] = None,
                 payload: typing.Optional[Payload] = None):
        """
        :param name: The name of this symbol.
        :param storage_kind: The storage kind of this symbol.
        :param uuid: The UUID of this Node,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        :param payload: The value this symbol points to.
            May be an address, a Node, or None.
        """

        super().__init__(uuid)
        self.name = name
        self.storage_kind = storage_kind
        self._payload = payload

    @property
    def value(self) -> typing.Optional[Value]:
        """The value of a Symbol, which is an integer or None.
        ``value`` and ``referent`` are mutually exclusive.
        """

        if not isinstance(self._payload, (Block, DataObject, ProxyBlock)):
            return self._payload
        return None

    @property
    def referent(self) -> typing.Optional[Referent]:
        """The object referred to by a Symbol, which is a
        Block, DataObject, ProxyBlock, or None.
        ``value`` and ``referent`` are mutually exclusive.
        """

        if isinstance(self._payload, (Block, DataObject, ProxyBlock)):
            return self._payload
        return None

    @value.setter
    def value(self, value: typing.Optional[Value]):
        self._payload = value

    @referent.setter
    def referent(self, referent: typing.Optional[Referent]):
        self._payload = referent

    @classmethod
    def _decode_protobuf(
        cls, proto_symbol: Symbol_pb2.Symbol, uuid: UUID
    ) -> "Symbol":
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

    def _to_protobuf(self) -> Symbol_pb2.Symbol:
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
        return (
            "Symbol("
            "uuid={uuid!r}, "
            "name={name!r}, "
            "storage_kind=Symbol.{storage_kind!s}, "
            "payload={_payload!r}, "
            ")".format(**self.__dict__)
        )
