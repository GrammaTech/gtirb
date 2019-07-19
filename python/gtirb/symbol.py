from enum import Enum
from uuid import UUID

import Symbol_pb2

from gtirb.block import Block, ProxyBlock
from gtirb.dataobject import DataObject
from gtirb.node import Node


class Symbol(Node):
    """
    Represents a Symbol, which maps a name to an object in the IR.
    """
    class StorageKind(Enum):
        """
        Indicates the storage kind of a Symbol.
        """
        Undefined = Symbol_pb2.StorageKind.Value('Storage_Undefined')
        Normal = Symbol_pb2.StorageKind.Value('Storage_Normal')
        Static = Symbol_pb2.StorageKind.Value('Storage_Static')
        Extern = Symbol_pb2.StorageKind.Value('Storage_Extern')
        Local = Symbol_pb2.StorageKind.Value('Storage_Local')

    def __init__(self,
                 name,
                 storage_kind=StorageKind.Undefined,
                 uuid=None):
        super().__init__(uuid)
        self.name = name
        self.storage_kind = storage_kind
        self._payload = None

    @property
    def value(self):
        if not isinstance(self._payload, (Block, DataObject, ProxyBlock)):
            return self._payload
        return None

    @property
    def referent(self):
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
        symbol = cls(name=proto_symbol.name,
                     uuid=uuid,
                     storage_kind=storage_kind)
        if proto_symbol.HasField('value'):
            symbol.value = proto_symbol.value
        if proto_symbol.HasField('referent_uuid'):
            referent_uuid = UUID(bytes=proto_symbol.referent_uuid)
            try:
                symbol.referent = Node.uuid_cache[referent_uuid]
            except KeyError as e:
                raise KeyError("Could not find referent UUID %s" % e)
        return symbol

    def to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        proto_symbol = Symbol_pb2.Symbol()
        proto_symbol.uuid = self.uuid.bytes
        if self.value is not None:
            proto_symbol.value = self.value
        elif self.referent is not None:
            proto_symbol.referent_uuid = self.referent.uuid.bytes
        proto_symbol.name = self.name
        proto_symbol.storage_kind = self.storage_kind.value
        return proto_symbol
