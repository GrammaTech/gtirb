import typing
from enum import Enum
from uuid import UUID

from .node import Node
from .proto import SymbolicExpression_pb2
from .symbol import Symbol
from .util import DeserializationError

AttributesCtorType = typing.Iterable[
    typing.Union[
        "SymbolicExpression_pb2.SymAttribute.ValueType",
        "SymbolicExpression.Attribute",
    ]
]


class SymbolicExpression:
    """Base class of symbolic expression types."""

    class Attribute(Enum):
        """Attributes representing a known property of a symbolic expression.
        See https://grammatech.github.io/gtirb/md__symbolic_expression.html
        """

        GOT = SymbolicExpression_pb2.SymAttribute.Value("GOT")
        GOTPC = SymbolicExpression_pb2.SymAttribute.Value("GOTPC")
        GOTOFF = SymbolicExpression_pb2.SymAttribute.Value("GOTOFF")
        GOTREL = SymbolicExpression_pb2.SymAttribute.Value("GOTREL")
        PLT = SymbolicExpression_pb2.SymAttribute.Value("PLT")
        PLTOFF = SymbolicExpression_pb2.SymAttribute.Value("PLTOFF")
        PCREL = SymbolicExpression_pb2.SymAttribute.Value("PCREL")
        SECREL = SymbolicExpression_pb2.SymAttribute.Value("SECREL")
        TLS = SymbolicExpression_pb2.SymAttribute.Value("TLS")
        TLSGD = SymbolicExpression_pb2.SymAttribute.Value("TLSGD")
        TLSLD = SymbolicExpression_pb2.SymAttribute.Value("TLSLD")
        TLSLDM = SymbolicExpression_pb2.SymAttribute.Value("TLSLDM")
        TLSCALL = SymbolicExpression_pb2.SymAttribute.Value("TLSCALL")
        TLSDESC = SymbolicExpression_pb2.SymAttribute.Value("TLSDESC")
        TPREL = SymbolicExpression_pb2.SymAttribute.Value("TPREL")
        TPOFF = SymbolicExpression_pb2.SymAttribute.Value("TPOFF")
        DTPREL = SymbolicExpression_pb2.SymAttribute.Value("DTPREL")
        DTPOFF = SymbolicExpression_pb2.SymAttribute.Value("DTPOFF")
        DTPMOD = SymbolicExpression_pb2.SymAttribute.Value("DTPMOD")
        NTPOFF = SymbolicExpression_pb2.SymAttribute.Value("NTPOFF")
        PAGE = SymbolicExpression_pb2.SymAttribute.Value("PAGE")
        PAGEOFF = SymbolicExpression_pb2.SymAttribute.Value("PAGEOFF")
        CALL = SymbolicExpression_pb2.SymAttribute.Value("CALL")
        LO = SymbolicExpression_pb2.SymAttribute.Value("LO")
        HI = SymbolicExpression_pb2.SymAttribute.Value("HI")
        HIGHER = SymbolicExpression_pb2.SymAttribute.Value("HIGHER")
        HIGHEST = SymbolicExpression_pb2.SymAttribute.Value("HIGHEST")
        GOTNTPOFF = SymbolicExpression_pb2.SymAttribute.Value("GOTNTPOFF")
        INDNTPOFF = SymbolicExpression_pb2.SymAttribute.Value("INDNTPOFF")
        G0 = SymbolicExpression_pb2.SymAttribute.Value("G0")
        G1 = SymbolicExpression_pb2.SymAttribute.Value("G1")
        G2 = SymbolicExpression_pb2.SymAttribute.Value("G2")
        G3 = SymbolicExpression_pb2.SymAttribute.Value("G3")
        UPPER16 = SymbolicExpression_pb2.SymAttribute.Value("UPPER16")
        LOWER16 = SymbolicExpression_pb2.SymAttribute.Value("LOWER16")
        LO12 = SymbolicExpression_pb2.SymAttribute.Value("LO12")
        LO15 = SymbolicExpression_pb2.SymAttribute.Value("LO15")
        LO14 = SymbolicExpression_pb2.SymAttribute.Value("LO14")
        HI12 = SymbolicExpression_pb2.SymAttribute.Value("HI12")
        HI21 = SymbolicExpression_pb2.SymAttribute.Value("HI21")
        S = SymbolicExpression_pb2.SymAttribute.Value("S")
        PG = SymbolicExpression_pb2.SymAttribute.Value("PG")
        NC = SymbolicExpression_pb2.SymAttribute.Value("NC")
        ABS = SymbolicExpression_pb2.SymAttribute.Value("ABS")
        PREL = SymbolicExpression_pb2.SymAttribute.Value("PREL")
        PREL31 = SymbolicExpression_pb2.SymAttribute.Value("PREL31")
        TARGET1 = SymbolicExpression_pb2.SymAttribute.Value("TARGET1")
        TARGET2 = SymbolicExpression_pb2.SymAttribute.Value("TARGET2")
        SBREL = SymbolicExpression_pb2.SymAttribute.Value("SBREL")
        TLSLDO = SymbolicExpression_pb2.SymAttribute.Value("TLSLDO")
        HI16 = SymbolicExpression_pb2.SymAttribute.Value("HI16")
        LO16 = SymbolicExpression_pb2.SymAttribute.Value("LO16")
        GPREL = SymbolicExpression_pb2.SymAttribute.Value("GPREL")
        DISP = SymbolicExpression_pb2.SymAttribute.Value("DISP")
        OFST = SymbolicExpression_pb2.SymAttribute.Value("OFST")
        H = SymbolicExpression_pb2.SymAttribute.Value("H")
        L = SymbolicExpression_pb2.SymAttribute.Value("L")
        HA = SymbolicExpression_pb2.SymAttribute.Value("HA")
        HIGH = SymbolicExpression_pb2.SymAttribute.Value("HIGH")
        HIGHA = SymbolicExpression_pb2.SymAttribute.Value("HIGHA")
        HIGHERA = SymbolicExpression_pb2.SymAttribute.Value("HIGHERA")
        HIGHESTA = SymbolicExpression_pb2.SymAttribute.Value("HIGHESTA")
        TOCBASE = SymbolicExpression_pb2.SymAttribute.Value("TOCBASE")
        TOC = SymbolicExpression_pb2.SymAttribute.Value("TOC")
        NOTOC = SymbolicExpression_pb2.SymAttribute.Value("NOTOC")

        def __repr__(self) -> str:
            return "SymbolicExpression.Attribute.%s" % self.name

        def __int__(self) -> int:
            return self.value

    def __init__(
        self,
        attributes: AttributesCtorType = set(),
    ):
        self.attributes = set(attributes)

    @property
    def symbols(self) -> typing.Iterable[Symbol]:
        """Get all the symbols involved with this symbolic expression,
        regardless of role.
        """

        raise NotImplementedError  # pragma: no cover

    def deep_eq(self, other: object) -> bool:
        raise NotImplementedError  # pragma: no cover

    def _attributes_repr(self) -> str:
        if not self.attributes:
            return "set()"
        else:
            return "{%s}" % ",".join(repr(a) for a in self.attributes)


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
        scale: int,
        offset: int,
        symbol1: Symbol,
        symbol2: Symbol,
        attributes: AttributesCtorType = set(),
    ):
        """
        :param scale: Constant scale factor.
        :param offset: Constant offset.
        :param symbol1: Symbol representing the base address.
        :param symbol2: Symbol to subtract from ``symbol1``.
        :param attributes: :class:`SymobolicExpression.Attribute`\\s of this
            expression.
        """
        super().__init__(attributes)
        self.scale = scale
        self.offset = offset
        self.symbol1 = symbol1
        self.symbol2 = symbol2

    @classmethod
    def _from_protobuf(
        cls,
        proto_symaddraddr: SymbolicExpression_pb2.SymAddrAddr,
        get_by_uuid: typing.Callable[[UUID], typing.Optional[Node]],
    ) -> "SymAddrAddr":
        symbol1_uuid = UUID(bytes=proto_symaddraddr.symbol1_uuid)
        symbol1 = get_by_uuid(symbol1_uuid)
        if not isinstance(symbol1, Symbol):
            raise DeserializationError(
                "SymAddrAddr: UUID %s is not a Symbol" % symbol1_uuid
            )
        symbol2_uuid = UUID(bytes=proto_symaddraddr.symbol2_uuid)
        symbol2 = get_by_uuid(symbol2_uuid)
        if not isinstance(symbol2, Symbol):
            raise DeserializationError(
                "SymAddrAddr: UUID %s is not a Symbol" % symbol2_uuid
            )
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

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, SymAddrAddr):
            return False
        return (
            self.scale == other.scale
            and self.offset == other.offset
            and self.symbol1.uuid == other.symbol1.uuid
            and self.symbol2.uuid == other.symbol2.uuid
            and self.attributes == other.attributes
        )

    def __hash__(self) -> int:
        return hash(
            (self.offset, self.scale, self.symbol1.uuid, self.symbol2.uuid)
        )

    def __repr__(self) -> str:
        return (
            "SymAddrAddr("
            "scale={scale!r}, "
            "offset={offset!r}, "
            "symbol1={symbol1!r}, "
            "symbol2={symbol2!r}, "
            "attributes={attributes_repr!s}, "
            ")"
        ).format(
            scale=self.scale,
            offset=self.offset,
            symbol1=self.symbol1,
            symbol2=self.symbol2,
            attributes_repr=self._attributes_repr(),
        )

    def deep_eq(self, other: object) -> bool:
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
    def symbols(self) -> typing.Iterable[Symbol]:
        yield self.symbol1
        yield self.symbol2


class SymAddrConst(SymbolicExpression):
    """Represents a symbolic expression of the form "Sym + Offset".

    :ivar ~.offset: Constant offset.
    :ivar ~.symbol: Symbol representing an address.
    """

    def __init__(
        self,
        offset: int,
        symbol: Symbol,
        attributes: AttributesCtorType = set(),
    ):
        """
        :param offset: Constant offset.
        :param symbol: Symbol representing an address.
        :param attributes: :class:`SymbolicExpression.Attribute`\\s of this
            expression.
        """
        super().__init__(attributes)
        self.offset = offset
        self.symbol = symbol

    @classmethod
    def _from_protobuf(
        cls,
        proto_symaddrconst: SymbolicExpression_pb2.SymAddrConst,
        get_by_uuid: typing.Callable[[UUID], typing.Optional[Node]],
    ) -> "SymAddrConst":
        symbol_uuid = UUID(bytes=proto_symaddrconst.symbol_uuid)
        symbol = get_by_uuid(symbol_uuid)
        if not isinstance(symbol, Symbol):
            raise DeserializationError(
                "SymAddrConst: UUID %s is not a Symbol" % symbol_uuid
            )
        return cls(proto_symaddrconst.offset, symbol)

    def _to_protobuf(self) -> SymbolicExpression_pb2.SymAddrConst:
        proto_symaddrconst = SymbolicExpression_pb2.SymAddrConst()
        proto_symaddrconst.offset = self.offset
        if self.symbol is not None:
            proto_symaddrconst.symbol_uuid = self.symbol.uuid.bytes
        return proto_symaddrconst

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, SymAddrConst):
            return False
        return (
            self.offset == other.offset
            and self.symbol.uuid == other.symbol.uuid
            and self.attributes == other.attributes
        )

    def __hash__(self) -> int:
        return hash((self.offset, self.symbol.uuid))

    def __repr__(self) -> str:
        return (
            "SymAddrConst("
            "offset={offset!r}, "
            "symbol={symbol!r}, "
            "attributes={attributes_repr!s}, "
            ")"
        ).format(
            offset=self.offset,
            symbol=self.symbol,
            attributes_repr=self._attributes_repr(),
        )

    def deep_eq(self, other: object) -> bool:
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, SymAddrConst):
            return False
        return (
            self.offset == other.offset
            and self.symbol.deep_eq(other.symbol)
            and self.attributes == other.attributes
        )

    @property
    def symbols(self) -> typing.Iterable[Symbol]:
        yield self.symbol
