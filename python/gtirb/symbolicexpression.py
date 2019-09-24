from uuid import UUID

import SymbolicExpression_pb2

from .node import Node


class SymAddrAddr:
    """Represents a symbolic operand of the form
    "(Sym1 - Sym2) / Scale + Offset".

    :param scale: how much the difference needs divided by
    :param offset: the fixed offset of the difference
    :param symbol1: the base symbol
    :param symbol2: the index symbol
    """

    def __init__(self, scale, offset, symbol1, symbol2):
        self.scale = scale
        self.offset = offset
        self.symbol1 = symbol1
        self.symbol2 = symbol2

    @classmethod
    def _from_protobuf(cls, proto_symaddraddr):
        symbol1 = Node._uuid_cache[UUID(bytes=proto_symaddraddr.symbol1_uuid)]
        symbol2 = Node._uuid_cache[UUID(bytes=proto_symaddraddr.symbol2_uuid)]
        return cls(
            proto_symaddraddr.scale, proto_symaddraddr.offset, symbol1, symbol2
        )

    def _to_protobuf(self):
        proto_symaddraddr = SymbolicExpression_pb2.SymAddrAddr()
        proto_symaddraddr.scale = self.scale
        proto_symaddraddr.offset = self.offset
        proto_symaddraddr.symbol1_uuid = self.symbol1.uuid.bytes
        proto_symaddraddr.symbol2_uuid = self.symbol2.uuid.bytes
        return proto_symaddraddr

    def __eq__(self, other):
        if not isinstance(other, SymAddrAddr):
            return False
        return (
            self.scale == other.scale
            and self.offset == other.offset
            and self.symbol1.uuid == other.symbol1.uuid
            and self.symbol2.uuid == other.symbol2.uuid
        )

    def __hash__(self):
        return hash(
            (self.offset, self.scale, self.symbol1.uuid, self.symbol2.uuid)
        )

    def __repr__(self):
        return (
            "SymAddrAddr("
            "scale={scale!r}, "
            "offset={offset!r}, "
            "symbol1={symbol1!r}, "
            "symbol2={symbol2!r}, "
            ")".format(**self.__dict__)
        )


class SymAddrConst:
    """Represents a symbolic operand of the form "Sym + Offset".

    :param offset: a fixed offset from the symbol
    :param symbol: the symbol to refer to
    """

    def __init__(self, offset, symbol):
        self.offset = offset
        self.symbol = symbol

    @classmethod
    def _from_protobuf(cls, proto_symaddrconst):
        symbol = Node._uuid_cache[UUID(bytes=proto_symaddrconst.symbol_uuid)]
        return cls(proto_symaddrconst.offset, symbol)

    def _to_protobuf(self):
        proto_symaddrconst = SymbolicExpression_pb2.SymAddrConst()
        proto_symaddrconst.offset = self.offset
        if self.symbol is not None:
            proto_symaddrconst.symbol_uuid = self.symbol.uuid.bytes
        return proto_symaddrconst

    def __eq__(self, other):
        if not isinstance(other, SymAddrConst):
            return False
        return (
            self.offset == other.offset
            and self.symbol.uuid == other.symbol.uuid
        )

    def __hash__(self):
        return hash((self.offset, self.symbol.uuid))

    def __repr__(self):
        return (
            "SymAddrConst("
            "offset={offset!r}, "
            "symbol={symbol!r}, "
            ")".format(**self.__dict__)
        )


class SymStackConst:
    """Represents a symbolic operand of the form "Sym + Offset",
    representing an offset from a stack variable.

    :param offset: a fixed offset from the symbol
    :param symbol: the symbol to refer to
    """

    def __init__(self, offset, symbol):
        self.offset = offset
        self.symbol = symbol

    @classmethod
    def _from_protobuf(cls, proto_symstackconst):
        symbol = Node._uuid_cache[UUID(bytes=proto_symstackconst.symbol_uuid)]
        return cls(proto_symstackconst.offset, symbol)

    def _to_protobuf(self):
        proto_symstackconst = SymbolicExpression_pb2.SymStackConst()
        proto_symstackconst.offset = self.offset
        if self.symbol is not None:
            proto_symstackconst.symbol_uuid = self.symbol.uuid.bytes
        return proto_symstackconst

    def __eq__(self, other):
        if not isinstance(other, SymStackConst):
            return False
        return (
            self.offset == other.offset
            and self.symbol.uuid == other.symbol.uuid
        )

    def __hash__(self):
        return hash((self.offset, self.symbol.uuid))

    def __repr__(self):
        return (
            "SymStackConst("
            "offset={offset!r}, "
            "symbol={symbol!r}, "
            ")".format(**self.__dict__)
        )
