from uuid import UUID

import SymbolicExpression_pb2

from .node import Node


class SymAddrAddr:
    """
    Represents a "symbolic operand" of the form
    "(Sym1 - Sym2) / Scale + Offset"
    """

    def __init__(self, scale, offset, symbol1, symbol2):
        self.scale = scale
        self.offset = offset
        self.symbol1 = symbol1
        self.symbol2 = symbol2

    @classmethod
    def _from_protobuf(cls, proto_symaddraddr):
        """
        Load this cls from protobuf object
        """
        symbol1 = Node._uuid_cache[UUID(bytes=proto_symaddraddr.symbol1_uuid)]
        symbol2 = Node._uuid_cache[UUID(bytes=proto_symaddraddr.symbol2_uuid)]
        return cls(proto_symaddraddr.scale,
                   proto_symaddraddr.offset,
                   symbol1,
                   symbol2)

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        proto_symaddraddr = SymbolicExpression_pb2.SymAddrAddr()
        proto_symaddraddr.scale = self.scale
        proto_symaddraddr.offset = self.offset
        proto_symaddraddr.symbol1_uuid = self.symbol1.uuid.bytes
        proto_symaddraddr.symbol2_uuid = self.symbol2.uuid.bytes
        return proto_symaddraddr

    def deep_eq(self, other):
        """Compare structural equality"""
        if not isinstance(other, SymAddrAddr):
            return False
        return self.scale == other.scale \
            and self.offset == other.offset \
            and self.symbol1.deep_eq(other.symbol1) \
            and self.symbol2.deep_eq(other.symbol2)


class SymAddrConst:
    """
    Represents a "symbolic operand" of the form "Sym + Offset".
    """
    def __init__(self, offset, symbol):
        self.offset = offset
        self.symbol = symbol

    @classmethod
    def _from_protobuf(cls, proto_symaddrconst):
        """
        Load this cls from protobuf object
        """
        symbol = Node._uuid_cache[UUID(bytes=proto_symaddrconst.symbol_uuid)]
        return cls(proto_symaddrconst.offset, symbol)

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        proto_symaddrconst = SymbolicExpression_pb2.SymAddrConst()
        proto_symaddrconst.offset = self.offset
        if self.symbol is not None:
            proto_symaddrconst.symbol_uuid = self.symbol.uuid.bytes
        return proto_symaddrconst

    def deep_eq(self, other):
        """Compare structural equality"""
        if not isinstance(other, SymAddrConst):
            return False
        return self.offset == other.offset \
            and self.symbol.deep_eq(other.symbol)


class SymStackConst:
    """
    Represents a "symbolic operand" of the form "Sym + Offset",
    representing an offset from a stack variable.
    """
    def __init__(self, offset, symbol):
        self.offset = offset
        self.symbol = symbol

    @classmethod
    def _from_protobuf(cls, proto_symstackconst):
        symbol = Node._uuid_cache[UUID(bytes=proto_symstackconst.symbol_uuid)]
        return cls(proto_symstackconst.offset, symbol)

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        proto_symstackconst = SymbolicExpression_pb2.SymStackConst()
        proto_symstackconst.offset = self.offset
        if self.symbol is not None:
            proto_symstackconst.symbol_uuid = self.symbol.uuid.bytes
        return proto_symstackconst

    def deep_eq(self, other):
        """Compare structural equality"""
        if not isinstance(other, SymStackConst):
            return False
        return self.offset == other.offset \
            and self.symbol.deep_eq(other.symbol)
