"""The IR is the core class for reading and writing GTIRB files.

    You can open a GTIRB Protobuf file and load it into an IR instance:

    >>> ir = IR.load_protobuf('filename.gtirb')

    And then you can write the IR instance as a Protobuf file:

    >>> ir.save_protobuf('filename.gtirb')
"""

import itertools
import typing
from uuid import UUID

from .auxdata import AuxData, AuxDataContainer
from .block import ByteBlock, CfgNode, CodeBlock, DataBlock, ProxyBlock
from .byteinterval import ByteInterval, SymbolicExpressionElement
from .cfg import CFG, Edge
from .module import Module
from .node import Node, _NodeMessage
from .proto import CFG_pb2, IR_pb2
from .section import Section
from .symbol import Symbol
from .util import (
    DictLike,
    ListWrapper,
    nodes_at,
    nodes_on,
    symbolic_expressions_at,
)
from .version import PROTOBUF_VERSION


class IR(AuxDataContainer):
    """A complete internal representation consisting of multiple Modules.

    :ivar ~.modules: A list of :class:`Module`\\s contained in the IR.
    :ivar ~.cfg: The IR's control flow graph.
    :ivar ~.version: The Protobuf version of this IR.
    """

    class _ModuleList(ListWrapper[Module]):
        def __init__(self, node: "IR", *args: typing.Iterable[Module]):
            self._node = node
            super().__init__(*args)

        def _remove(self, v: Module) -> None:
            v._ir = None
            v._remove_from_uuid_cache(self._node._local_uuid_cache)

        def _add(self, v: Module) -> None:
            if v._ir is not None:
                v._ir.modules.remove(v)
            v._ir = self._node
            v._add_to_uuid_cache(self._node._local_uuid_cache)

    def __init__(
        self,
        *,
        modules: typing.Iterable[Module] = [],
        aux_data: DictLike[str, AuxData] = {},
        cfg: typing.Iterable[Edge] = set(),
        version: int = PROTOBUF_VERSION,
        uuid: typing.Optional[UUID] = None,
    ):
        """
        :param modules: A list of Modules contained in the IR.
        :param cfg: A set of :class:`Edge`\\s representing the IR's control
            flow graph. Defaults to being empty.
        :param aux_data: The initial auxiliary data to be associated
            with the object, as a mapping from names to
            :class:`gtirb.AuxData`. Defaults to being empty.
        :param version: The Protobuf version of this IR.
        :param uuid: The UUID of this ``IR``,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        """

        self._local_uuid_cache: typing.Dict[UUID, Node] = {}
        # Modules are decoded before the aux data, since the UUID decoder
        # checks Node's cache.
        self.modules = IR._ModuleList(self, modules)
        self.cfg = CFG(cfg)
        self.version = version
        super().__init__(aux_data, uuid)
        self._local_uuid_cache[self.uuid] = self

    @classmethod
    def _decode_protobuf(
        cls, proto_ir: _NodeMessage, uuid: UUID, _: typing.Optional["IR"]
    ) -> "IR":
        assert isinstance(proto_ir, IR_pb2.IR)
        if proto_ir.version != PROTOBUF_VERSION:
            raise ValueError(
                "Attempt to decode IR of version %s (expected version %s)"
                % (proto_ir.version, PROTOBUF_VERSION)
            )

        ir = cls(version=proto_ir.version, uuid=uuid)
        ir.modules.extend(
            Module._from_protobuf(m, ir) for m in proto_ir.modules
        )
        ir.cfg = CFG._from_protobuf(proto_ir.cfg.edges, ir)
        ir.aux_data.update(
            AuxDataContainer._read_protobuf_aux_data(proto_ir.aux_data, ir)
        )
        return ir

    def _to_protobuf(self) -> IR_pb2.IR:
        proto_ir = IR_pb2.IR()
        proto_ir.uuid = self.uuid.bytes
        proto_ir.version = self.version
        proto_ir.modules.extend(m._to_protobuf() for m in self.modules)
        proto_cfg = CFG_pb2.CFG()
        proto_cfg.vertices.extend(v.uuid.bytes for v in self.cfg_nodes)
        proto_cfg.edges.extend(self.cfg._to_protobuf())
        proto_ir.cfg.CopyFrom(proto_cfg)
        self._write_protobuf_aux_data(proto_ir.aux_data)
        return proto_ir

    def deep_eq(self, other: object) -> bool:
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, IR) or not super().deep_eq(other):
            return False
        self_modules = sorted(self.modules, key=lambda m: m.uuid)
        other_modules = sorted(other.modules, key=lambda m: m.uuid)
        if not len(self_modules) == len(other_modules):
            return False
        for self_module, other_module in zip(self_modules, other_modules):
            if not self_module.deep_eq(other_module):
                return False
        return self.version == other.version and self.cfg.deep_eq(other.cfg)

    @staticmethod
    def load_protobuf_file(protobuf_file: typing.BinaryIO) -> "IR":
        """Load IR from a Protobuf object.

        Use this function when you have a Protobuf object already loaded,
        and you want to parse it as a GTIRB IR.
        If the Protobuf object is stored in a file,
        use :func:`gtirb.IR.load_protobuf` instead.

        :param protobuf_file: A byte stream encoding a GTIRB Protobuf message.
        :returns: An IR object representing the same
            information that is contained in ``protobuf_file``.
        """

        ir = IR_pb2.IR()
        ir.ParseFromString(protobuf_file.read())
        return IR._from_protobuf(ir, None)

    @staticmethod
    def load_protobuf(file_name: str) -> "IR":
        """Load IR from a Protobuf file at the specified path.

        :param file_name: The path to the Protobuf file.
        :returns: A Python GTIRB IR object.
        """
        with open(file_name, "rb") as f:
            return IR.load_protobuf_file(f)

    def save_protobuf_file(self, protobuf_file: typing.BinaryIO) -> None:
        """Save ``self`` to a Protobuf object.

        :param protobuf_file: The byte stream to write the GTIRB Protobuf
            message to.
        """

        protobuf_file.write(self._to_protobuf().SerializeToString())

    def save_protobuf(self, file_name: str) -> None:
        """Save ``self`` to a Protobuf file at the specified path.

        :param file_name: The file path at which to
            save the Protobuf representation of ``self``.
        """
        with open(file_name, "wb") as f:
            self.save_protobuf_file(f)

    def __repr__(self) -> str:
        return (
            "IR("
            "uuid={uuid!r}, "
            "modules={modules!r}, "
            "cfg={cfg!r}, "
            "version={version}, "
            ")".format(**self.__dict__)
        )

    @property
    def proxy_blocks(self) -> typing.Iterator[ProxyBlock]:
        """The :class:`ProxyBlock`\\s in this IR."""

        return itertools.chain.from_iterable(m.proxies for m in self.modules)

    @property
    def sections(self) -> typing.Iterator[Section]:
        """The :class:`Section`\\s in this IR."""

        return itertools.chain.from_iterable(m.sections for m in self.modules)

    @property
    def symbols(self) -> typing.Iterator[Symbol]:
        """The :class:`Symbol`\\s in this IR."""

        return itertools.chain.from_iterable(m.symbols for m in self.modules)

    @property
    def byte_intervals(self) -> typing.Iterator[ByteInterval]:
        """The :class:`ByteInterval`\\s in this IR."""

        return itertools.chain.from_iterable(
            m.byte_intervals for m in self.modules
        )

    @property
    def byte_blocks(self) -> typing.Iterator[ByteBlock]:
        """The :class:`ByteBlock`\\s in this IR."""

        return itertools.chain.from_iterable(
            m.byte_blocks for m in self.modules
        )

    @property
    def code_blocks(self) -> typing.Iterator[CodeBlock]:
        """The :class:`CodeBlock`\\s in this IR."""

        return itertools.chain.from_iterable(
            m.code_blocks for m in self.modules
        )

    @property
    def data_blocks(self) -> typing.Iterator[DataBlock]:
        """The :class:`DataBlock`\\s in this IR."""

        return itertools.chain.from_iterable(
            m.data_blocks for m in self.modules
        )

    @property
    def cfg_nodes(self) -> typing.Iterator[CfgNode]:
        """The :class:`CfgNode`\\s in this IR."""

        return itertools.chain.from_iterable(m.cfg_nodes for m in self.modules)

    def sections_on(
        self, addrs: typing.Union[int, range]
    ) -> typing.Iterable[Section]:
        """Finds all the sections that overlap an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_on(self.sections, addrs)

    def sections_at(
        self, addrs: typing.Union[int, range]
    ) -> typing.Iterable[Section]:
        """Finds all the sections that begin at an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_at(self.sections, addrs)

    def byte_intervals_on(
        self, addrs: typing.Union[int, range]
    ) -> typing.Iterable[ByteInterval]:
        """Finds all the byte intervals that overlap an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return itertools.chain.from_iterable(
            m.byte_intervals_on(addrs) for m in self.modules
        )

    def byte_intervals_at(
        self, addrs: typing.Union[int, range]
    ) -> typing.Iterable[ByteInterval]:
        """Finds all the byte intervals that begin at an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return itertools.chain.from_iterable(
            m.byte_intervals_at(addrs) for m in self.modules
        )

    def byte_blocks_on(
        self, addrs: typing.Union[int, range]
    ) -> typing.Iterable[ByteBlock]:
        """Finds all the byte blocks that overlap an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return itertools.chain.from_iterable(
            m.byte_blocks_on(addrs) for m in self.modules
        )

    def byte_blocks_at(
        self, addrs: typing.Union[int, range]
    ) -> typing.Iterable[ByteBlock]:
        """Finds all the byte blocks that begin at an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return itertools.chain.from_iterable(
            m.byte_blocks_at(addrs) for m in self.modules
        )

    def code_blocks_on(
        self, addrs: typing.Union[int, range]
    ) -> typing.Iterable[CodeBlock]:
        """Finds all the code blocks that overlap an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return itertools.chain.from_iterable(
            m.code_blocks_on(addrs) for m in self.modules
        )

    def code_blocks_at(
        self, addrs: typing.Union[int, range]
    ) -> typing.Iterable[CodeBlock]:
        """Finds all the code blocks that begin at an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return itertools.chain.from_iterable(
            m.code_blocks_at(addrs) for m in self.modules
        )

    def data_blocks_on(
        self, addrs: typing.Union[int, range]
    ) -> typing.Iterable[DataBlock]:
        """Finds all the data blocks that overlap an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return itertools.chain.from_iterable(
            m.data_blocks_on(addrs) for m in self.modules
        )

    def data_blocks_at(
        self, addrs: typing.Union[int, range]
    ) -> typing.Iterable[DataBlock]:
        """Finds all the data blocks that begin at an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return itertools.chain.from_iterable(
            m.data_blocks_at(addrs) for m in self.modules
        )

    def symbolic_expressions_at(
        self, addrs: typing.Union[int, range]
    ) -> typing.Iterable[SymbolicExpressionElement]:
        """Finds all the symbolic expressions that begin at an address or
        range of addresses.

        :param addrs: Either a ``range`` object or a single address.
        :returns: Yields ``(interval, offset, symexpr)`` tuples for every
            symbolic expression in the range.
        """

        return symbolic_expressions_at(self.modules, addrs)

    def get_by_uuid(self, uuid: UUID) -> typing.Optional[Node]:
        """Look up a node by its UUID.

        This method will find any node currently attached to this IR.
        It will not find any nodes attached to other IRs, or not attached to
        any IR.

        :param uuid: The UUID to look up.
        :returns: The Node this UUID corresponds to, or None if no node exists
            with that UUID.
        """

        return self._local_uuid_cache.get(uuid)
