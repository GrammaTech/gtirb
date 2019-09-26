from uuid import UUID
import typing

import SymbolicExpression_pb2

from .node import Node
from .symbol import Symbol


class SymAddrAddr:
    """Represents a symbolic operand of the form
    "(Sym1 - Sym2) / Scale + Offset".

    :ivar scale: How much the difference needs divided by in bytes.
    :ivar offset: The fixed offset of the difference in bytes.
    :ivar symbol1: The base symbol.
    :ivar symbol2: The index symbol.
    """

    scale: int
    offset: int
    symbol1: Symbol
    symbol2: Symbol

    def __init__(
        self, scale: int, offset: int, symbol1: Symbol, symbol2: Symbol
    ):
        """
        :param scale: How much the difference needs divided by in bytes.
        :param offset: The fixed offset of the difference in bytes.
        :param symbol1: The base symbol.
        :param symbol2: The index symbol.
        """

        self.scale = scale
        self.offset = offset
        self.symbol1 = symbol1
        self.symbol2 = symbol2

    @classmethod
    def _from_protobuf(
        cls, proto_symaddraddr: SymbolicExpression_pb2.SymAddrAddr
    ) -> "SymAddrAddr":
        symbol1 = Node._uuid_cache[UUID(bytes=proto_symaddraddr.symbol1_uuid)]
        symbol2 = Node._uuid_cache[UUID(bytes=proto_symaddraddr.symbol2_uuid)]
        return cls(
            proto_symaddraddr.scale, proto_symaddraddr.offset, symbol1, symbol2
        )

    def _to_protobuf(self) -> SymbolicExpression_pb2.SymAddrAddr:
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

    :ivar offset: A fixed offset from the symbol in bytes.
    :ivar symbol: The symbol to refer to.
    """

    offset: int
    symbol: Symbol

    def __init__(self, offset: int, symbol: Symbol):
        """
        :param offset: A fixed offset from the symbol in bytes.
        :param symbol: The symbol to refer to.
        """

        self.offset = offset
        self.symbol = symbol

    @classmethod
    def _from_protobuf(
        cls, proto_symaddrconst: SymbolicExpression_pb2.SymAddrConst
    ) -> "SymAddrConst":
        symbol = Node._uuid_cache[UUID(bytes=proto_symaddrconst.symbol_uuid)]
        return cls(proto_symaddrconst.offset, symbol)

    def _to_protobuf(self) -> SymbolicExpression_pb2.SymAddrConst:
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

    :ivar offset: A fixed offset from the symbol in bytes.
    :ivar symbol: The symbol to refer to.
    """

    offset: int
    symbol: Symbol

    def __init__(self, offset: int, symbol: Symbol):
        """
        :param offset: A fixed offset from the symbol in bytes.
        :param symbol: The symbol to refer to.
        """

        self.offset = offset
        self.symbol = symbol

    @classmethod
    def _from_protobuf(
        cls, proto_symstackconst: SymbolicExpression_pb2.SymStackConst
    ) -> "SymStackConst":
        symbol = Node._uuid_cache[UUID(bytes=proto_symstackconst.symbol_uuid)]
        return cls(proto_symstackconst.offset, symbol)

    def _to_protobuf(self) -> SymbolicExpression_pb2.SymStackConst:
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
        return ("SymStackConst("
                "offset={offset!r}, "
                "symbol={symbol!r}, "
                ")".format(**self.__dict__))


SymbolicOperand = typing.Union[SymAddrAddr, SymAddrConst, SymStackConst]
"""A type hint for any symbolic operand type."""
