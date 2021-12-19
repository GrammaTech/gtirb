import typing
from uuid import UUID

from .node import Node, _NodeMessage
from .proto import CodeBlock_pb2, DataBlock_pb2, ProxyBlock_pb2
from .util import _IndexedAttribute

if typing.TYPE_CHECKING:  # pragma: no cover
    # Ignore flake8 "imported but unused" errors.
    from .byteinterval import ByteInterval  # noqa: F401
    from .cfg import Edge  # noqa: F401
    from .ir import IR  # noqa: F401
    from .module import Module  # noqa: F401
    from .section import Section  # noqa: F401
    from .symbol import Symbol  # noqa: F401


class Block(Node):
    """The base class for blocks. Symbols may have references to any subclass
    of Block.
    """

    @property
    def references(self) -> typing.Iterator["Symbol"]:
        """Get all the symbols that refer to this block."""

        raise NotImplementedError  # pragma: no cover

    def _add_to_uuid_cache(self, cache: typing.Dict[UUID, Node]) -> None:
        """Update the UUID cache when this node is added."""

        cache[self.uuid] = self

    def _remove_from_uuid_cache(self, cache: typing.Dict[UUID, Node]) -> None:
        """Update the UUID cache when this node is removed."""

        del cache[self.uuid]


class ByteBlock(Block):
    """The base class for blocks that belong to a :class:`ByteInterval` and
    store their bytes there.

    :ivar ~.size: The size of the block in bytes.
    :ivar ~.offset: The offset from the beginning of the byte interval to which
        this block belongs. Multiple blocks in the same interval may have the
        same offset.
    """

    size = _IndexedAttribute[int]()(lambda self: self.byte_interval)
    offset = _IndexedAttribute[int]()(lambda self: self.byte_interval)

    def __init__(
        self,
        *,
        size: int = 0,
        offset: int = 0,
        uuid: typing.Optional[UUID] = None,
        byte_interval: typing.Optional["ByteInterval"] = None,
    ):
        """
        :param size: The size of the data object in bytes.
        :param offset: The offset from the beginning of the byte interval to
            which this block belongs.
        :param uuid: The UUID of this ``ByteBlock``,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        :param byte_interval: The :class:`ByteInterval` this block belongs to.
        """

        super().__init__(uuid=uuid)
        self._byte_interval: typing.Optional["ByteInterval"] = None
        self.size = size
        self.offset = offset
        # Use the property setter to ensure correct invariants.
        self.byte_interval = byte_interval

    @property
    def byte_interval(self) -> typing.Optional["ByteInterval"]:
        """The :class:`ByteInterval` this block belongs to."""

        return self._byte_interval

    @byte_interval.setter
    def byte_interval(self, value: typing.Optional["ByteInterval"]) -> None:
        if self._byte_interval is not None:
            self._byte_interval.blocks.discard(self)
        if value is not None:
            value.blocks.add(self)

    def deep_eq(self, other: object) -> bool:
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, ByteBlock):
            return False
        return (
            self.offset == other.offset
            and self.uuid == other.uuid
            and self.size == other.size
        )

    @property
    def contents(self) -> bytes:
        """Get the bytes in this block."""

        if self.byte_interval is None:
            return b""
        return self.byte_interval.contents[
            self.offset : self.offset + self.size
        ]

    @property
    def address(self) -> typing.Optional[int]:
        """Get the address of this block, or None if not present."""

        if self.byte_interval is None or self.byte_interval.address is None:
            return None
        return self.byte_interval.address + self.offset

    @property
    def references(self) -> typing.Iterator["Symbol"]:
        if (
            self.byte_interval is None
            or self.byte_interval.section is None
            or self.byte_interval.section.module is None
        ):
            return iter(())
        return (
            s
            for s in self.byte_interval.section.module.symbols
            if s.referent == self
        )

    @property
    def section(self) -> typing.Optional["Section"]:
        """Get the section this node ultimately belongs to."""
        if self.byte_interval is None:
            return None
        return self.byte_interval.section

    @property
    def module(self) -> typing.Optional["Module"]:
        """Get the module this node ultimately belongs to."""
        if self.section is None:
            return None
        return self.section.module

    @property
    def ir(self) -> typing.Optional["IR"]:
        """Get the IR this node ultimately belongs to."""
        if self.module is None:
            return None
        return self.module.ir

    def contains_offset(self, offset: int) -> bool:
        """Indicate if the provided offset is within this block."""
        return self.offset <= offset < (self.offset + self.size)

    def contains_address(self, address: int) -> bool:
        """Indicate if the provided address is within this block.
        Returns False if the block has no address.
        """
        byte_interval = self.byte_interval
        if byte_interval is not None:
            base = byte_interval.address
            if base is not None:
                return self.contains_offset(address - base)
        return False


class CfgNode(Block):
    """The base class for blocks that may appear as vertices in the CFG."""

    @property
    def incoming_edges(self) -> typing.Iterable["Edge"]:
        """Get the edges that point to this CFG node."""

        raise NotImplementedError  # pragma: no cover

    @property
    def outgoing_edges(self) -> typing.Iterable["Edge"]:
        """Get the edges that start at this CFG node."""

        raise NotImplementedError  # pragma: no cover


class DataBlock(ByteBlock):
    """Represents a data object, possibly symbolic."""

    def __init__(
        self,
        *,
        size: int = 0,
        offset: int = 0,
        uuid: typing.Optional[UUID] = None,
        byte_interval: typing.Optional["ByteInterval"] = None,
    ):
        """
        :param size: The size of the data object in bytes.
        :param offset: The offset from the beginning of the byte interval to
            which this block belongs.
        :param uuid: The UUID of this ``DataBlock``,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        :param byte_interval: The :class:`ByteInterval` this block belongs to.
        """

        super().__init__(
            size=size, offset=offset, uuid=uuid, byte_interval=byte_interval
        )

    @classmethod
    def _decode_protobuf(
        cls,
        proto_dataobject: _NodeMessage,
        uuid: UUID,
        ir: typing.Optional["IR"],
    ) -> "DataBlock":
        assert ir
        assert isinstance(proto_dataobject, DataBlock_pb2.DataBlock)
        b = cls(size=proto_dataobject.size, uuid=uuid)
        b._add_to_uuid_cache(ir._local_uuid_cache)
        return b

    def _to_protobuf(self) -> DataBlock_pb2.DataBlock:
        proto_dataobject = DataBlock_pb2.DataBlock()
        proto_dataobject.uuid = self.uuid.bytes
        proto_dataobject.size = self.size
        return proto_dataobject

    def __repr__(self) -> str:
        return (
            "DataBlock("
            "uuid={uuid!r}, "
            "size={size}, "
            "offset={offset}, "
            ")".format(uuid=self.uuid, size=self.size, offset=self.offset)
        )


class CodeBlock(ByteBlock, CfgNode):
    """A basic block in the binary.

    Does not directly store data bytes, which are kept in a
    :class:`ByteInterval`.

    :ivar ~.decode_mode: The decode mode of the block,
        used in some ISAs to differentiate between sub-ISAs
        (e.g. differentiating blocks written in ARM and Thumb).
    """

    def __init__(
        self,
        *,
        decode_mode: int = 0,
        size: int = 0,
        offset: int = 0,
        uuid: typing.Optional[UUID] = None,
        byte_interval: typing.Optional["ByteInterval"] = None,
    ):
        """
        :param size: The length of the block in bytes.
        :param decode_mode: The decode mode of the block,
            used in some ISAs to differentiate between sub-ISAs
            (e.g. differentiating blocks written in ARM and Thumb).
            Defaults to 0.
        :param offset: The offset from the beginning of the byte interval to
            which this block belongs.
        :param uuid: The UUID of this ``CodeBlock``,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        :param byte_interval: The :class:`ByteInterval` this block belongs to.
        """

        super().__init__(
            size=size, offset=offset, uuid=uuid, byte_interval=byte_interval
        )
        self.decode_mode = decode_mode

    @classmethod
    def _decode_protobuf(
        cls, proto_block: _NodeMessage, uuid: UUID, ir: typing.Optional["IR"],
    ) -> "CodeBlock":
        assert ir
        assert isinstance(proto_block, CodeBlock_pb2.CodeBlock)
        b = cls(
            decode_mode=proto_block.decode_mode,
            size=proto_block.size,
            uuid=uuid,
        )
        b._add_to_uuid_cache(ir._local_uuid_cache)
        return b

    def _to_protobuf(self) -> CodeBlock_pb2.CodeBlock:
        proto_block = CodeBlock_pb2.CodeBlock()
        proto_block.uuid = self.uuid.bytes
        proto_block.size = self.size
        proto_block.decode_mode = self.decode_mode
        return proto_block

    def deep_eq(self, other: object) -> bool:
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, CodeBlock):
            return False
        return super().deep_eq(other) and self.decode_mode == other.decode_mode

    def __repr__(self) -> str:
        return (
            "CodeBlock("
            "uuid={uuid!r}, "
            "size={size}, "
            "offset={offset}, "
            "decode_mode={decode_mode}, "
            ")".format(
                uuid=self.uuid,
                size=self.size,
                offset=self.offset,
                decode_mode=self.decode_mode,
            )
        )

    @property
    def incoming_edges(self) -> typing.Iterator["Edge"]:
        if self.ir is None:
            return iter(())
        return self.ir.cfg.in_edges(self)

    @property
    def outgoing_edges(self) -> typing.Iterator["Edge"]:
        if self.ir is None:
            return iter(())
        return self.ir.cfg.out_edges(self)


class ProxyBlock(CfgNode):
    """A placeholder that serves as the endpoint (source or target) of a
    :class:`gtirb.Edge`.

    ProxyBlock objects allow the construction of CFG edges to or from
    another node. For example, a call to a function in another module
    may be represented by a :class:`gtirb.Edge` that originates at the
    calling :class:`gtirb.CodeBlock` and targets a ProxyBlock. Another
    example would be a :class:`gtirb.Edge` that represents an indirect
    jump whose target is not known.

    A ProxyBlock does not represent any instructions and so has neither
    an address nor a size.
    """

    def __init__(
        self,
        *,
        uuid: typing.Optional[UUID] = None,
        module: typing.Optional["Module"] = None,
    ):
        super().__init__(uuid=uuid)
        self._module: typing.Optional["Module"] = None
        # Use the property setter to ensure correct invariants.
        self.module = module

    @classmethod
    def _decode_protobuf(
        cls, proto_proxy: _NodeMessage, uuid: UUID, ir: typing.Optional["IR"],
    ) -> "ProxyBlock":
        assert ir
        assert isinstance(proto_proxy, ProxyBlock_pb2.ProxyBlock)
        b = cls(uuid=uuid)
        b._add_to_uuid_cache(ir._local_uuid_cache)
        return b

    def _to_protobuf(self) -> ProxyBlock_pb2.ProxyBlock:
        proto_proxyblock = ProxyBlock_pb2.ProxyBlock()
        proto_proxyblock.uuid = self.uuid.bytes
        return proto_proxyblock

    def deep_eq(self, other: object) -> bool:
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, ProxyBlock):
            return False
        return self.uuid == other.uuid

    def __repr__(self) -> str:
        return "ProxyBlock(" "uuid={uuid!r}, " ")".format(**self.__dict__)

    @property
    def module(self) -> typing.Optional["Module"]:
        return self._module

    @module.setter
    def module(self, value: typing.Optional["Module"]) -> None:
        if self._module is not None:
            self._module.proxies.discard(self)
        if value is not None:
            value.proxies.add(self)

    @property
    def references(self) -> typing.Iterator["Symbol"]:
        if self.module is None:
            return iter(())
        return (s for s in self.module.symbols if s.referent == self)

    @property
    def incoming_edges(self) -> typing.Iterator["Edge"]:
        if self.ir is None:
            return iter(())
        return self.ir.cfg.in_edges(self)

    @property
    def outgoing_edges(self) -> typing.Iterator["Edge"]:
        if self.ir is None:
            return iter(())
        return self.ir.cfg.out_edges(self)

    @property
    def ir(self) -> typing.Optional["IR"]:
        """Get the IR this node ultimately belongs to."""
        if self.module is None:
            return None
        return self.module.ir
