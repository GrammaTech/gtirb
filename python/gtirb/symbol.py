from enum import Enum
from uuid import UUID, uuid4

import Symbol_pb2

from gtirb.block import Block, ProxyBlock
from gtirb.dataobject import DataObject


class Symbol:
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
                 uuid=None,
                 uuid_cache=None):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
        if uuid_cache is not None:
            uuid_cache[uuid] = self
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

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        symbol = Symbol_pb2.Symbol()
        symbol.uuid = self.uuid.bytes
        if self.value is not None:
            symbol.value = self.value
        elif self.referent is not None:
            symbol.referent_uuid = self.referent.uuid.bytes
        symbol.name = self.name
        symbol.storage_kind = self.storage_kind.value
        return symbol

    @classmethod
    def _from_protobuf(cls, proto_symbol, uuid_cache):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=proto_symbol.uuid)
        if uuid in uuid_cache:
            return uuid_cache[uuid]
        symbol = cls(name=proto_symbol.name,
                     uuid=uuid,
                     storage_kind=Symbol.StorageKind(
                         proto_symbol.storage_kind),
                     uuid_cache=uuid_cache)
        if proto_symbol.HasField('value'):
            symbol.value = proto_symbol.value
        if proto_symbol.HasField('referent_uuid'):
            referent_uuid = UUID(bytes=proto_symbol.referent_uuid)
            try:
                symbol.referent = uuid_cache[referent_uuid]
            except KeyError as e:
                raise KeyError("Could not find referent UUID %s" % (e))
        return symbol
