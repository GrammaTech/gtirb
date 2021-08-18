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

        # General bit/byte masking and indexing operations:
        Part0 = SymbolicExpression_pb2.SEAttributeFlag.Value("Part0")
        Part1 = SymbolicExpression_pb2.SEAttributeFlag.Value("Part1")
        Part2 = SymbolicExpression_pb2.SEAttributeFlag.Value("Part2")
        Part3 = SymbolicExpression_pb2.SEAttributeFlag.Value("Part3")
        Adjusted = SymbolicExpression_pb2.SEAttributeFlag.Value("Adjusted")
        Hi = SymbolicExpression_pb2.SEAttributeFlag.Value("Hi")
        Lo = SymbolicExpression_pb2.SEAttributeFlag.Value("Lo")
        Lo12 = SymbolicExpression_pb2.SEAttributeFlag.Value("Lo12")

        # GOT and GOT-relative attributes:
        GotRef = SymbolicExpression_pb2.SEAttributeFlag.Value("GotRef")
        GotRelPC = SymbolicExpression_pb2.SEAttributeFlag.Value("GotRelPC")
        GotRelGot = SymbolicExpression_pb2.SEAttributeFlag.Value("GotRelGot")
        AddrRelGot = SymbolicExpression_pb2.SEAttributeFlag.Value("AddrRelGot")
        GotRelAddr = SymbolicExpression_pb2.SEAttributeFlag.Value("GotRelAddr")
        GotPage = SymbolicExpression_pb2.SEAttributeFlag.Value("GotPage")
        GotPageOfst = SymbolicExpression_pb2.SEAttributeFlag.Value(
            "GotPageOfst"
        )
        GotOff = SymbolicExpression_pb2.SEAttributeFlag.Value("GotOff")

        # PLT specific attributes:
        PltRef = SymbolicExpression_pb2.SEAttributeFlag.Value("PltRef")

        # TLS specific attributes:
        TpOff = SymbolicExpression_pb2.SEAttributeFlag.Value("TpOff")
        NtpOff = SymbolicExpression_pb2.SEAttributeFlag.Value("NtpOff")
        DtpOff = SymbolicExpression_pb2.SEAttributeFlag.Value("DtpOff")
        TlsGd = SymbolicExpression_pb2.SEAttributeFlag.Value("TlsGd")
        TlsLd = SymbolicExpression_pb2.SEAttributeFlag.Value("TlsLd")

        # Attribute modifiers:
        Abs = SymbolicExpression_pb2.SEAttributeFlag.Value("Abs")
        Signed = SymbolicExpression_pb2.SEAttributeFlag.Value("Signed")
        NoOverflowCheck = SymbolicExpression_pb2.SEAttributeFlag.Value(
            "NoOverflowCheck"
        )

    def __init__(
        self, attributes=set(),
    ):
        # type: ("AttributesCtorType") -> None
        self.attributes = set(attributes)

    @property
    def symbols(self):
        # type: () -> typing.Iterable[Symbol]
        """Get all the symbols involved with this symbolic expression,
        regardless of role.
        """

        return ()

    def _attributes_repr(self):
        # type: () -> str
        if not self.attributes:
            return "set()"
        else:
            return "{%s}" % ",".join(
                "SymbolicExpression.Attribute.%s" % a.name
                for a in self.attributes
            )


if typing.TYPE_CHECKING:
    AttributesCtorType = typing.Iterable[SymbolicExpression.Attribute]


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
        attributes=set(),  # type: AttributesCtorType
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
            "attributes={attributes_repr!s}, "
            ")"
        ).format(attributes_repr=self._attributes_repr(), **self.__dict__)

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

    @property
    def symbols(self):
        yield self.symbol1
        yield self.symbol2


class SymAddrConst(SymbolicExpression):
    """Represents a symbolic expression of the form "Sym + Offset".

    :ivar ~.offset: Constant offset.
    :ivar ~.symbol: Symbol representing an address.
    """

    def __init__(self, offset, symbol, attributes=set()):
        # type: (int, Symbol, AttributesCtorType) -> None
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
            "attributes={attributes_repr!s}, "
            ")"
        ).format(attributes_repr=self._attributes_repr(), **self.__dict__)

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

    @property
    def symbols(self):
        yield self.symbol
