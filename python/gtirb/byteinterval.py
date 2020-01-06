import ByteInterval_pb2
import SymbolicExpression_pb2
import typing
from uuid import UUID

from .node import Node
from .block import ByteBlock, CodeBlock, DataBlock
from .symbolicexpression import (
    SymAddrAddr,
    SymAddrConst,
    SymStackConst,
    SymbolicOperand,
)
from .util import DictLike, SetWrapper


class ByteInterval(Node):
    """A contiguous region of bytes in a binary.

    A ByteInterval defines a relative ordering for a group of
    :class:`ByteBlock`\\s, optionally at a fixed address in memory. It also
    stores the bytes associated with these blocks.

    If two blocks are in two different ByteIntervals, then it should be
    considered safe (that is, preserving of program semantics) to move one
    block relative to the other in memory. If two blocks are in the same
    ByteInterval, then it should be considered unknown if moving the two blocks
    relative to one another in memory is a safe operation.

    :ivar address: The fixed address of this interval, if present. If this
        field is present, it may indicate the original address at which this
        interval was located at in memory, or it may indicate that this block's
        address is fixed and must not be changed. If this field is not present,
        it indicates that the interval is free to be moved around in memory
        while preserving program semantics.
    :ivar size: The size of this interval in bytes. If this number is greater
        than ``initialized_size``, this indicates that the high addresses taken
        up by this interval consist of uninitialized bytes. This often occurs
        in BSS sections, where data is zero-initialized rather than stored as
        zeroes in the binary.
    :ivar initialized_size: The number of initialized bytes in this interval.
        Not all bytes in this interval may correspond to bytes physically
        stored in the underlying file format. This can occur, for example, in
        BSS sections, which are zero-initialized at loadtime, but these zeroes
        are not stored in the file itself. If this number is smaller than
        ``size``, this indicates that any bytes past this number are
        unitialized bytes with values determined at loadtime. As such, all
        bytes past this number in this interval's byte vector are truncated
        when saving to file.
    :ivar contents: The bytes stored in this interval.
    :ivar blocks: A set of all :class:`ByteBlock`\\s in this interval.
    :ivar symbolic_operands: A mapping, from offset in the interval, to a
        :class:`SymbolicOperand` in the interval.
    """

    class _BlockSet(SetWrapper):
        def __init__(self, node, *args):
            self._node = node  # type: ByteInterval
            super().__init__(*args)

        def add(self, v):
            if v._byte_interval is not None:
                v._byte_interval.blocks.discard(v)
            v._byte_interval = self._node
            return super().add(v)

        def discard(self, v):
            v._byte_interval = None
            return super().discard(v)

    def __init__(
        self,
        *,
        address=None,  # type: typing.Optional[int]
        size=None,  # type: typing.Optional[int]
        initialized_size=None,  # type: typing.Optional[int]
        contents=b"",  # type: typing.ByteString
        blocks=(),  # type: typing.Iterable[ByteBlock]
        symbolic_operands={},  # type: DictLike[int, SymbolicOperand]
        uuid=None,  # type: typing.Optional[UUID]
    ):
        """
        :param address: The fixed address of this interval, if present.
        :param size: The size of this interval in bytes.
        :param initialized_size: The number of initialized bytes in this
            interval.
        :param contents: The bytes stored in this interval.
        :param blocks: A set of all :class:`ByteBlock`\\s in this interval.
        :param symbolic_operands: A mapping, from offset in the interval, to a
            :class:`SymbolicOperand` in the interval.
        :param uuid: The UUID of this ``ByteInterval``,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        """

        if size is None:
            size = len(contents)
            if initialized_size is not None and initialized_size > size:
                size = initialized_size
        if initialized_size is None:
            initialized_size = size
        if size > len(contents):
            contents = contents + b"\0" * (size - len(contents))
        if initialized_size > size:
            raise ValueError("initialized_size must be <= size!")

        super().__init__(uuid=uuid)
        self.address = address  # type: typing.Optional[int]
        self.size = size  # type: int
        self.initialized_size = initialized_size  # type: int
        self.contents = bytearray(contents)  # type: bytearray
        self.blocks = ByteInterval._BlockSet(
            self, blocks
        )  # type: typing.Set[ByteBlock]
        self.symbolic_operands = dict(
            symbolic_operands
        )  # type: typing.Dict[int, SymbolicOperand]
        self._section = None  # type: typing.Optional["Section"]
        self._proto_interval = (
            None
        )  # type: typing.Optional[ByteInterval_pb2.ByteInterval]

    @classmethod
    def _decode_protobuf(cls, proto_interval, uuid):
        # type: (ByteInterval_pb2.ByteInterval, UUID) -> ByteInterval

        def decode_block(proto_block):
            if proto_block.HasField("code"):
                block = CodeBlock._from_protobuf(proto_block.code)
            elif proto_block.HasField("data"):
                block = DataBlock._from_protobuf(proto_block.data)
            else:
                raise TypeError(
                    "Unknown type inside proto block: %s"
                    % proto_block.WhichOneof("value")
                )

            block.offset = proto_block.offset
            return block

        # we do not decode symbolic expressions yet, because symbols have
        # not yet been decoded at this point.
        result = cls(
            address=proto_interval.address
            if proto_interval.has_address
            else None,
            size=proto_interval.size,
            contents=proto_interval.contents,
            blocks=(decode_block(b) for b in proto_interval.blocks),
            uuid=uuid,
        )
        # we store the interval here so we can use it later, when
        # _decode_symbolic_expressions is called.
        result._proto_interval = proto_interval
        return result

    def _decode_symbolic_expressions(self):
        """Called by modules after symbols are decoded, but before the module
        is done decoding.
        """

        def decode_symbolic_expression(proto_expr):
            if proto_expr.HasField("stack_const"):
                return SymStackConst._from_protobuf(proto_expr.stack_const)
            elif proto_expr.HasField("addr_const"):
                return SymAddrConst._from_protobuf(proto_expr.addr_const)
            elif proto_expr.HasField("addr_addr"):
                return SymAddrAddr._from_protobuf(proto_expr.addr_addr)
            else:
                raise TypeError(
                    "Unknown type inside proto sym expr: %s"
                    % proto_expr.WhichOneof("value")
                )

        self.symbolic_operands = {
            i: decode_symbolic_expression(v)
            for i, v in self._proto_interval.symbolic_expressions.items()
        }
        self._proto_interval = None

    def _to_protobuf(self):
        # type: () -> ByteInterval_pb2.ByteInterval
        proto_interval = ByteInterval_pb2.ByteInterval()

        proto_interval.uuid = self.uuid.bytes
        if self.address is None:
            proto_interval.has_address = False
        else:
            proto_interval.has_address = True
            proto_interval.address = self.address
        proto_interval.size = self.size
        proto_interval.contents = bytes(self.contents[: self.initialized_size])

        for block in self.blocks:
            proto_block = ByteInterval_pb2.Block()
            proto_block.offset = block.offset
            if isinstance(block, CodeBlock):
                proto_block.code.CopyFrom(block._to_protobuf())
            elif isinstance(block, DataBlock):
                proto_block.data.CopyFrom(block._to_protobuf())
            else:
                raise TypeError(
                    "Unknown block type in interval: %s" % type(block)
                )
            proto_interval.blocks.append(proto_block)

        for k, v in self.symbolic_operands.items():
            sym_exp = SymbolicExpression_pb2.SymbolicExpression()
            if isinstance(v, SymStackConst):
                sym_exp.stack_const.CopyFrom(v._to_protobuf())
            elif isinstance(v, SymAddrConst):
                sym_exp.addr_const.CopyFrom(v._to_protobuf())
            elif isinstance(v, SymAddrAddr):
                sym_exp.addr_addr.CopyFrom(v._to_protobuf())
            else:
                raise ValueError(
                    "Expected sym expr type in interval: %s" % type(v)
                )
            proto_interval.symbolic_expressions[k].CopyFrom(sym_exp)

        return proto_interval

    @property
    def section(self):
        # type: () -> typing.Optional["Section"]
        """The :class:`Section` this interval belongs to."""

        return self._section

    @section.setter
    def section(self, value):
        # type: (typing.Optional["Section"]) -> None
        if self._section is not None:
            self._section.byte_intervals.discard(self)
        if value is not None:
            value.byte_intervals.add(self)

    def deep_eq(self, other):
        # type: (typing.Any) -> bool
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, ByteInterval):
            return False
        return (
            self.uuid == other.uuid
            and self.address == other.address
            and self.contents == other.contents
            and self.initialized_size == other.initialized_size
            and len(self.blocks) == len(other.blocks)
            and all(
                self_node.deep_eq(other_node)
                for self_node, other_node in zip(
                    sorted(self.blocks, key=lambda b: b.uuid),
                    sorted(other.blocks, key=lambda b: b.uuid),
                )
            )
            and len(self.symbolic_operands) == len(other.symbolic_operands)
            and all(
                self_kv[0] == other_kv[0] and self_kv[1].deep_eq(other_kv[1])
                for self_kv, other_kv in zip(
                    sorted(
                        self.symbolic_operands.items(), key=lambda kv: kv[0]
                    ),
                    sorted(
                        other.symbolic_operands.items(), key=lambda kv: kv[0]
                    ),
                )
            )
        )

    def __repr__(self):
        # type: () -> str
        return (
            "ByteInterval("
            "uuid={uuid!r}, "
            "address={address}, "
            "size={size}, "
            "initialized_size={initialized_size}, "
            "contents={contents!r}, "
            "blocks={blocks!r}, "
            "symbolic_operands={symbolic_operands!r}, "
            ")".format(**self.__dict__)
        )
