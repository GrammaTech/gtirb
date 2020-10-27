import typing
from enum import Enum
from uuid import UUID

from .node import Node
from .proto import SymbolicExpression_pb2
from .symbol import Symbol


class SymbolicExpression:
    """Base class of symbolic expression types."""

    class Attribute(Enum):
        """Attributes representing a known property of a symbolic
        expression."""

        Part0 = SymbolicExpression_pb2.SEAttributeFlag.Value("Part0")
        Part1 = SymbolicExpression_pb2.SEAttributeFlag.Value("Part1")
        Part2 = SymbolicExpression_pb2.SEAttributeFlag.Value("Part2")
        Part3 = SymbolicExpression_pb2.SEAttributeFlag.Value("Part3")
        Adjusted = SymbolicExpression_pb2.SEAttributeFlag.Value("Adjusted")
        GotRef = SymbolicExpression_pb2.SEAttributeFlag.Value("GotRef")
        GotRelPC = SymbolicExpression_pb2.SEAttributeFlag.Value("GotRelPC")
        GotRelGot = SymbolicExpression_pb2.SEAttributeFlag.Value("GotRelGot")
        AddrRelGot = SymbolicExpression_pb2.SEAttributeFlag.Value("AddrRelGot")
        GotRelAddr = SymbolicExpression_pb2.SEAttributeFlag.Value("GotRelAddr")
        GotPage = SymbolicExpression_pb2.SEAttributeFlag.Value("GotPage")
        GotPageOfst = SymbolicExpression_pb2.SEAttributeFlag.Value(
            "GotPageOfst"
        )
        PltRef = SymbolicExpression_pb2.SEAttributeFlag.Value("PltRef")

    def __init__(
        self, attributes=set(),
    ):
        self.attributes = set(attributes)


class SymAddrAddr(SymbolicExpression):
    """Represents a symbolic expression of the form
    "(Sym1 - Sym2) / Scale + Offset".

    :ivar ~.scale: Constant scale factor.
    :ivar ~.offset: Constant offset.
    :ivar ~.symbol1: Symbol representing the base address.
    :ivar ~.symbol2: Symbol to subtract from ``symbol1``.
    """

    def __init__(
        self,
        scale,  # type: int
        offset,  # type: int
        symbol1,  # type: Symbol
        symbol2,  # type: Symbol
        attributes=set(),
    ):
        # type: (...) -> None
        """
        :param scale: Constant scale factor.
        :param offset: Constant offset.
        :param symbol1: Symbol representing the base address.
        :param symbol2: Symbol to subtract from ``symbol1``.
        :param attributes: :class:`SymobolicExpression.Attribute`\\s of this
            expression.
        """
        super().__init__(attributes)
        self.scale = scale  # type: int
        self.offset = offset  # type: int
        self.symbol1 = symbol1  # type: Symbol
        self.symbol2 = symbol2  # type: Symbol

    @classmethod
    def _from_protobuf(
        cls,
        proto_symaddraddr,  # type: SymbolicExpression_pb2.SymAddrAddr
        get_by_uuid,  # type: typing.Callable[[UUID], Node]
    ):
        # type: (...) -> SymAddrAddr
        symbol1 = get_by_uuid(UUID(bytes=proto_symaddraddr.symbol1_uuid))
        symbol2 = get_by_uuid(UUID(bytes=proto_symaddraddr.symbol2_uuid))
        return cls(
            proto_symaddraddr.scale, proto_symaddraddr.offset, symbol1, symbol2
        )

    def _to_protobuf(self):
        # type: () -> SymbolicExpression_pb2.SymAddrAddr
        proto_symaddraddr = SymbolicExpression_pb2.SymAddrAddr()
        proto_symaddraddr.scale = self.scale
        proto_symaddraddr.offset = self.offset
        proto_symaddraddr.symbol1_uuid = self.symbol1.uuid.bytes
        proto_symaddraddr.symbol2_uuid = self.symbol2.uuid.bytes
        return proto_symaddraddr

    def __eq__(self, other):
        # type: (typing.Any) -> bool
        if not isinstance(other, SymAddrAddr):
            return False
        return (
            self.scale == other.scale
            and self.offset == other.offset
            and self.symbol1.uuid == other.symbol1.uuid
            and self.symbol2.uuid == other.symbol2.uuid
            and self.attributes == other.attributes
        )

    def __hash__(self):
        # type: () -> int
        return hash(
            (self.offset, self.scale, self.symbol1.uuid, self.symbol2.uuid)
        )

    def __repr__(self):
        # type: () -> str
        return (
            "SymAddrAddr("
            "scale={scale!r}, "
            "offset={offset!r}, "
            "symbol1={symbol1!r}, "
            "symbol2={symbol2!r}, "
            ")".format(**self.__dict__)
        )

    def deep_eq(self, other):
        # type: (typing.Any) -> bool
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, SymAddrAddr):
            return False
        return (
            self.scale == other.scale
            and self.offset == other.offset
            and self.symbol1.deep_eq(other.symbol1)
            and self.symbol2.deep_eq(other.symbol2)
            and self.attributes == other.attributes
        )


class SymAddrConst(SymbolicExpression):
    """Represents a symbolic expression of the form "Sym + Offset".

    :ivar ~.offset: Constant offset.
    :ivar ~.symbol: Symbol representing an address.
    """

    def __init__(self, offset, symbol, attributes=set()):
        # type: (int,Symbol) -> None
        """
        :param offset: Constant offset.
        :param symbol: Symbol representing an address.
        :param attributes: :class:`SymbolicExpression.Attribute`\\s of this
            expression.
        """
        super().__init__(attributes)
        self.offset = offset  # type: int
        self.symbol = symbol  # type: Symbol

    @classmethod
    def _from_protobuf(
        cls,
        proto_symaddrconst,  # type: SymbolicExpression_pb2.SymAddrConst
        get_by_uuid,  # type: typing.Callable[[UUID], Node]
    ):
        # type: (...) -> SymAddrConst
        symbol = get_by_uuid(UUID(bytes=proto_symaddrconst.symbol_uuid))
        return cls(proto_symaddrconst.offset, symbol)

    def _to_protobuf(self):
        # type: () -> SymbolicExpression_pb2.SymAddrConst
        proto_symaddrconst = SymbolicExpression_pb2.SymAddrConst()
        proto_symaddrconst.offset = self.offset
        if self.symbol is not None:
            proto_symaddrconst.symbol_uuid = self.symbol.uuid.bytes
        return proto_symaddrconst

    def __eq__(self, other):
        # type: (typing.Any) -> bool
        if not isinstance(other, SymAddrConst):
            return False
        return (
            self.offset == other.offset
            and self.symbol.uuid == other.symbol.uuid
            and self.attributes == other.attributes
        )

    def __hash__(self):
        # type: () -> int
        return hash((self.offset, self.symbol.uuid))

    def __repr__(self):
        # type: () -> str
        return (
            "SymAddrConst("
            "offset={offset!r}, "
            "symbol={symbol!r}, "
            ")".format(**self.__dict__)
        )

    def deep_eq(self, other):
        # type: (typing.Any) -> bool
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, SymAddrConst):
            return False
        return (
            self.offset == other.offset
            and self.symbol.deep_eq(other.symbol)
            and self.attributes == other.attributes
        )


class SymStackConst(SymbolicExpression):
    """Represents a symbolic expression of the form "Sym + Offset",
    representing an offset from a stack variable.

    :ivar ~.offset: Constant offset.
    :ivar ~.symbol: Symbol representing a stack variable.
    """

    def __init__(self, offset, symbol, attributes=set()):
        # type: (int,Symbol) -> None
        """
        :param offset: Constant offset.
        :param symbol: Symbol representing a stack variable.
        :param attributes: :class:`SymbolicExpression.Attribute`\\s of this
            expression.
        """

        super().__init__(attributes)
        self.offset = offset  # type: int
        self.symbol = symbol  # type: Symbol

    @classmethod
    def _from_protobuf(
        cls,
        proto_symstackconst,  # type: SymbolicExpression_pb2.SymStackConst
        get_by_uuid,  # type: typing.Callable[[UUID], Node]
    ):
        # type: (...) -> SymStackConst
        symbol = get_by_uuid(UUID(bytes=proto_symstackconst.symbol_uuid))
        return cls(proto_symstackconst.offset, symbol)

    def _to_protobuf(self):
        # type: () -> SymbolicExpression_pb2.SymStackConst
        proto_symstackconst = SymbolicExpression_pb2.SymStackConst()
        proto_symstackconst.offset = self.offset
        if self.symbol is not None:
            proto_symstackconst.symbol_uuid = self.symbol.uuid.bytes
        return proto_symstackconst

    def __eq__(self, other):
        # type: (typing.Any) -> bool
        if not isinstance(other, SymStackConst):
            return False
        return (
            self.offset == other.offset
            and self.symbol.uuid == other.symbol.uuid
            and self.attributes == other.attributes
        )

    def __hash__(self):
        # type: () -> int
        return hash((self.offset, self.symbol.uuid))

    def __repr__(self):
        # type: () -> str
        return (
            "SymStackConst("
            "offset={offset!r}, "
            "symbol={symbol!r}, "
            ")".format(**self.__dict__)
        )

    def deep_eq(self, other):
        # type: (typing.Any) -> bool
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, SymStackConst):
            return False
        return (
            self.offset == other.offset
            and self.symbol.deep_eq(other.symbol)
            and self.attributes == other.attributes
        )
