from enum import Enum
from uuid import UUID

import Symbol_pb2

from .block import Block, ProxyBlock
from .dataobject import DataObject
from .node import Node


class Symbol(Node):
    """Represents a symbol, which maps a name to an object in the IR.

    :param name: the name of this symbol
    :param storage_kind: the storage kind of this symbol
    :param payload: an optional value this symbol points to.
        May be an address, a Node, or None
    :param uuid: the UUID of this Node
    """

    class StorageKind(Enum):
        """
        Indicates the storage kind of a Symbol.
        """

        Undefined = Symbol_pb2.StorageKind.Value("Storage_Undefined")
        Normal = Symbol_pb2.StorageKind.Value("Storage_Normal")
        Static = Symbol_pb2.StorageKind.Value("Storage_Static")
        Extern = Symbol_pb2.StorageKind.Value("Storage_Extern")
        Local = Symbol_pb2.StorageKind.Value("Storage_Local")

    def __init__(
        self, name, storage_kind=StorageKind.Undefined, uuid=None, payload=None
    ):
        super().__init__(uuid)
        self.name = name
        self.storage_kind = storage_kind
        self._payload = payload

    @property
    def value(self):
        """The value of a Symbol, which is an integer or None.
        ``value`` and ``referent`` are mutually exclusive.
        """

        if not isinstance(self._payload, (Block, DataObject, ProxyBlock)):
            return self._payload
        return None

    @property
    def referent(self):
        """The object referred to by a Symbol, which is a
        Block, DataObject, ProxyBlock, or None.
        ``value`` and ``referent`` are mutually exclusive.
        """

        if isinstance(self._payload, (Block, DataObject, ProxyBlock)):
            return self._payload
        return None

    @value.setter
    def value(self, value):
        self._payload = value

    @referent.setter
    def referent(self, referent):
        self._payload = referent

    @classmethod
    def _decode_protobuf(cls, proto_symbol, uuid):
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
