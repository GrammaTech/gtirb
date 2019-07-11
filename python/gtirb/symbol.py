from enum import Enum
from uuid import UUID, uuid4

import Symbol_pb2


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
                 uuid=None,
                 name='',
                 storage_kind=StorageKind.Undefined,
                 value=0,
                 referent=None,
                 uuid_cache=None):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
        if uuid_cache is not None:
            uuid_cache[uuid] = self
        self.value = value
        self.referent = referent
        self.name = name
        self.storage_kind = storage_kind

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = Symbol_pb2.Symbol()
        ret.uuid = self.uuid.bytes

        if self.value is not None:
            ret.value = self.value

        if self.referent is not None:
            ret.referent_uuid = self.referent.uuid.bytes

        ret.name = self.name
        ret.storage_kind = self.storage_kind.value
        return ret

    @classmethod
    def _from_protobuf(cls, symbol, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=symbol.uuid)
        if uuid_cache is not None and uuid in uuid_cache:
            return uuid_cache[uuid]
        value = None
        referent = None
        if symbol.HasField('value'):
            value = symbol.value
        if symbol.HasField('referent_uuid'):
            referent_uuid = UUID(bytes=symbol.referent_uuid)
            referent = None
            if uuid_cache is not None:
                referent = uuid_cache.get(referent_uuid)
        return cls(uuid, symbol.name, Symbol.StorageKind(symbol.storage_kind),
                   value, referent, uuid_cache)
