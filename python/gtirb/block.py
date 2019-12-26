import CodeBlock_pb2
import DataBlock_pb2
import ProxyBlock_pb2
import typing
from uuid import UUID

from .node import Node


class Block(Node):
    """The base class for blocks. Symbols may have references to any subclass
    of Block.
    """


class ByteBlock(Block):
    """The base class for blocks that belong to a :class:`ByteInterval` and
    store thier bytes there.

    :ivar offset: The offset from the beginning of the byte interval to which
        this block belongs. Multiple blocks in the same interval may have the
        same offset.
    """

    def __init__(self, *, offset=0, uuid=None):
        """
        :param offset: The offset from the beginning of the byte interval to
            which this block belongs.
        :param uuid: The UUID of this ``ByteBlock``,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        """

        super().__init__(uuid=uuid)
        self.offset = offset  # type: int
        self._byte_interval = None  # type: typing.Optional["ByteInterval"]

    @property
    def byte_interval(self):
        # type: () -> typing.Optional["ByteInterval"]
        """The :class:`ByteInterval` this block belongs to."""

        return self._byte_interval

    @byte_interval.setter
    def byte_interval(self, value):
        # type: (typing.Optional["ByteInterval"]) -> None
        if self._byte_interval is not None:
            self._byte_interval.blocks.discard(self)
        if value is not None:
            value.blocks.add(self)


class CfgNode(Block):
    """The base class for blocks that may appear as vertices in the CFG."""


class DataBlock(ByteBlock):
    """Represents a data object, possibly symbolic.

    :ivar size: The size of the data object in bytes.
    """

    def __init__(self, size, offset=0, uuid=None):
        # type: (int, int, typing.Optional[UUID]) -> None
        """
        :param size: The size of the data object in bytes.
        :param offset: The offset from the beginning of the byte interval to
            which this block belongs.
        :param uuid: The UUID of this ``DataBlock``,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        """

        super().__init__(offset=offset, uuid=uuid)
        self.size = size

    @classmethod
    def _decode_protobuf(cls, proto_dataobject, uuid):
        # type: (DataBlock_pb2.DataBlock, uuid.UUID) -> DataBlock
        return cls(size=proto_dataobject.size, uuid=uuid)

    def _to_protobuf(self):
        # type: () -> DataBlock_pb2.DataBlock
        proto_dataobject = DataBlock_pb2.DataBlock()
        proto_dataobject.uuid = self.uuid.bytes
        proto_dataobject.size = self.size
        return proto_dataobject

    def deep_eq(self, other):
        # type: (typing.Any) -> bool
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, DataBlock):
            return False
        return self.uuid == other.uuid and self.size == other.size

    def __repr__(self):
        # type: () -> str
        return (
            "DataBlock("
            "uuid={uuid!r}, "
            "size={size}, "
            "offset={offset}, "
            ")".format(**self.__dict__)
        )


class CodeBlock(ByteBlock, CfgNode):
    """A basic block in the binary.

    Does not directly store data bytes, which are kept in a
    :class:`ByteInterval`.

    :ivar size: The length of the block in bytes.
    :ivar decode_mode: The decode mode of the block,
        used in some ISAs to differentiate between sub-ISAs
        (e.g. differentiating blocks written in ARM and Thumb).
    """

    def __init__(
        self,
        size,  # type: int
        *,
        decode_mode=0,  # type: int
        offset=0,
        uuid=None  # type: typing.Optional[UUID]
    ):
        # type: (...) -> None
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
        """

        super().__init__(offset=offset, uuid=uuid)
        self.size = size  # type: int
        self.decode_mode = decode_mode  # type: int

    @classmethod
    def _decode_protobuf(cls, proto_block, uuid):
        # type: (CodeBlock_pb2.CodeBlock, UUID) -> CodeBlock
        return cls(
            decode_mode=proto_block.decode_mode,
            size=proto_block.size,
            uuid=uuid,
        )

    def _to_protobuf(self):
        # type: () -> CodeBlock_pb2.CodeBlock
        proto_block = CodeBlock_pb2.CodeBlock()
        proto_block.uuid = self.uuid.bytes
        proto_block.size = self.size
        proto_block.decode_mode = self.decode_mode
        return proto_block

    def deep_eq(self, other):
        # type: (typing.Any) -> bool
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, CodeBlock):
            return False
        return (
            self.uuid == other.uuid
            and self.size == other.size
            and self.decode_mode == other.decode_mode
        )

    def __repr__(self):
        # type: () -> str
        return (
            "CodeBlock("
            "uuid={uuid!r}, "
            "size={size}, "
            "offset={offset}, "
            "decode_mode={decode_mode}, "
            ")".format(**self.__dict__)
        )


class ProxyBlock(CfgNode):
    """A placeholder to serve as the endpoint of a CFG edge.

    A ProxyBlock exists in the CFG so that edges to or from another
    node may be constructed. For example, a call to a function in
    another module may be represented by an edge that originates at
    the calling block and targets a proxy. Another example would be an
    edge to represent an indirect jump whose target is not known.

    ProxyBlocks do not represent any instructions and so have neither
    an address nor a size.
    """

    def __init__(self, *, uuid=None):
        super().__init__(uuid=uuid)
        self._module = None  # type: "Module"

    @classmethod
    def _decode_protobuf(cls, proto_proxy, uuid):
        # type: (ProxyBlock_pb2.ProxyBlock, UUID) -> ProxyBlock
        return cls(uuid=uuid)

    def _to_protobuf(self):
        # type: () -> ProxyBlock_pb2.ProxyBlock
        proto_proxyblock = ProxyBlock_pb2.ProxyBlock()
        proto_proxyblock.uuid = self.uuid.bytes
        return proto_proxyblock

    def deep_eq(self, other):
        # type: (typing.Any) -> bool
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, ProxyBlock):
            return False
        return self.uuid == other.uuid

    def __repr__(self):
        # type: () -> str
        return "ProxyBlock(" "uuid={uuid!r}, " ")".format(**self.__dict__)

    @property
    def module(self):
        # type: () -> "Module"
        return self._module

    @module.setter
    def module(self, value):
        # type: ("Module") -> None
        if self._module is not None:
            self._module.proxies.discard(self)
        if value is not None:
            value.proxies.add(self)
