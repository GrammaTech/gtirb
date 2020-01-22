"""The IR is the core class for reading and writing GTIRB files.

    You can open a GTIRB Protobuf file and load it into an IR instance:

    >>> ir = IR.load_protobuf('filename.gtirb')

    And then you can write the IR instance as a Protobuf file:

    >>> ir.save_protobuf('filename.gtirb')
"""

import CFG_pb2
import IR_pb2
import typing
import itertools
from uuid import UUID

from .auxdata import AuxData, AuxDataContainer
from .block import CodeBlock, DataBlock, ProxyBlock, CfgNode, ByteBlock
from .byteinterval import ByteInterval
from .cfg import Edge
from .module import Module
from .section import Section
from .symbol import Symbol
from .util import DictLike, ListWrapper, nodes_at, nodes_in
from .version import PROTOBUF_VERSION


class IR(AuxDataContainer):
    """A complete internal representation consisting of multiple Modules.

    :ivar modules: A list of :class:`Module`\\s contained in the IR.
    :ivar cfg: A set of :class:`Edge`\\s representing the IR's control
        flow graph.
    :ivar version: The Protobuf version of this IR.
    """

    class _ModuleList(ListWrapper[Module]):
        def __init__(self, node, *args):
            self._node = node  # type: IR
            super().__init__(*args)

        def _remove(self, v):
            v._ir = None

        def _add(self, v):
            if v._ir is not None:
                v._ir.modules.remove(v)
            v._ir = self._node

        def __setitem__(self, i, v):
            self._remove(self[i])
            super().__setitem__(i, v)
            self._add(v)

        def __delitem__(self, i):
            self._remove(self[i])
            super().__delitem__(i)

        def insert(self, i, v):
            self._add(v)
            return super().insert(i, v)

    def __init__(
        self,
        *,
        modules=list(),  # type: typing.Iterable[Module]
        aux_data=dict(),  # type: DictLike[str, AuxData]
        cfg=set(),  # type: typing.Iterable[Edge]
        version=PROTOBUF_VERSION,  # type: int
        uuid=None,  # type: typing.Optional[UUID]
    ):
        # type: (...) -> None
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

        # Modules are decoded before the aux data, since the UUID decoder
        # checks Node's cache.
        self.modules = IR._ModuleList(
            self, modules
        )  # type: typing.List[Module]
        self.cfg = set(cfg)  # type: typing.Set[Edge]
        self.version = version  # type: int
        super().__init__(aux_data, uuid)

    @classmethod
    def _decode_protobuf(cls, proto_ir, uuid):
        # type: (IR_pb2.IR, UUID) -> IR
        if proto_ir.version != PROTOBUF_VERSION:
            raise ValueError(
                "Attempt to decode IR of version %s (expected version %s)"
                % (proto_ir.version, PROTOBUF_VERSION)
            )
        modules = [Module._from_protobuf(m) for m in proto_ir.modules]
        cfg = [Edge._from_protobuf(e) for e in proto_ir.cfg.edges]
        aux_data = AuxDataContainer._read_protobuf_aux_data(proto_ir)
        return cls(
            modules=modules,
            aux_data=aux_data,
            cfg=cfg,
            version=proto_ir.version,
            uuid=uuid,
        )

    def _to_protobuf(self):
        # type: () -> IR_pb2.IR
        proto_ir = IR_pb2.IR()
        proto_ir.uuid = self.uuid.bytes
        proto_ir.version = self.version
        proto_ir.modules.extend(m._to_protobuf() for m in self.modules)
        proto_cfg = CFG_pb2.CFG()
        proto_cfg.vertices.extend(v.uuid.bytes for v in self.cfg_nodes)
        proto_cfg.edges.extend(e._to_protobuf() for e in self.cfg)
        proto_ir.cfg.CopyFrom(proto_cfg)
        self._write_protobuf_aux_data(proto_ir)
        return proto_ir

    def deep_eq(self, other):
        # type: (typing.Any) -> bool
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
        self_edges = sorted(
            self.cfg, key=lambda e: (e.source.uuid, e.target.uuid)
        )
        other_edges = sorted(
            other.cfg, key=lambda e: (e.source.uuid, e.target.uuid)
        )
        if not len(self_edges) == len(other_edges):
            return False
        for self_edge, other_edge in zip(self_edges, other_edges):
            if not self_edge.deep_eq(other_edge):
                return False
        return self.version == other.version

    @staticmethod
    def load_protobuf_file(protobuf_file):
        # type: (typing.BinaryIO) -> IR
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
        return IR._from_protobuf(ir)

    @staticmethod
    def load_protobuf(file_name):
        # type: (str) -> IR
        """Load IR from a Protobuf file at the specified path.

        :param file_name: The path to the Protobuf file.
        :returns: A Python GTIRB IR object.
        """
        with open(file_name, "rb") as f:
            return IR.load_protobuf_file(f)

    def save_protobuf_file(self, protobuf_file):
        # type: (typing.BinaryIO) -> None
        """Save ``self`` to a Protobuf object.

        :param protobuf_file: The byte stream to write the GTIRB Protobuf
            message to.
        """

        protobuf_file.write(self._to_protobuf().SerializeToString())

    def save_protobuf(self, file_name):
        # type: (str) -> None
        """Save ``self`` to a Protobuf file at the specified path.

        :param file_name: The file path at which to
            save the Protobuf representation of ``self``.
        """
        with open(file_name, "wb") as f:
            self.save_protobuf_file(f)

    def __repr__(self):
        # type: () -> str
        return (
            "IR("
            "uuid={uuid!r}, "
            "modules={modules!r}, "
            "cfg={cfg!r}, "
            "version={version}, "
            ")".format(**self.__dict__)
        )

    @property
    def proxy_blocks(self):
        # type: () -> typing.Iterator[ProxyBlock]
        """The :class:`ProxyBlock`\\s in this IR."""

        return itertools.chain.from_iterable(m.proxies for m in self.modules)

    @property
    def sections(self):
        # type: () -> typing.Iterator[Section]
        """The :class:`Section`\\s in this IR."""

        return itertools.chain.from_iterable(m.sections for m in self.modules)

    @property
    def symbols(self):
        # type: () -> typing.Iterator[Symbol]
        """The :class:`Symbol`\\s in this IR."""

        return itertools.chain.from_iterable(m.symbols for m in self.modules)

    @property
    def byte_intervals(self):
        # type: () -> typing.Iterator[ByteInterval]
        """The :class:`ByteInterval`\\s in this IR."""

        return itertools.chain.from_iterable(
            m.byte_intervals for m in self.modules
        )

    @property
    def byte_blocks(self):
        # type: () -> typing.Iterator[ByteBlock]
        """The :class:`ByteBlock`\\s in this IR."""

        return itertools.chain.from_iterable(
            m.byte_blocks for m in self.modules
        )

    @property
    def code_blocks(self):
        # type: () -> typing.Iterator[CodeBlock]
        """The :class:`CodeBlock`\\s in this IR."""

        return itertools.chain.from_iterable(
            m.code_blocks for m in self.modules
        )

    @property
    def data_blocks(self):
        # type: () -> typing.Iterator[DataBlock]
        """The :class:`DataBlock`\\s in this IR."""

        return itertools.chain.from_iterable(
            m.data_blocks for m in self.modules
        )

    @property
    def cfg_nodes(self):
        # type: () -> typing.Iterator[CfgNode]
        """The :class:`CfgNode`\\s in this IR."""

        return itertools.chain.from_iterable(m.cfg_nodes for m in self.modules)

    def modules_in(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[Module]
        """Finds all the modules that overlap an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_in(self.modules, addrs)

    def modules_at(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[Module]
        """Finds all the modules that begin at an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_at(self.modules, addrs)

    def sections_in(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[Section]
        """Finds all the sections that overlap an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_in(self.sections, addrs)

    def sections_at(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[Section]
        """Finds all the sections that begin at an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_at(self.sections, addrs)

    def byte_intervals_in(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[ByteInterval]
        """Finds all the byte intervals that overlap an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_in(self.byte_intervals, addrs)

    def byte_intervals_at(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[ByteInterval]
        """Finds all the byte intervals that begin at an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_at(self.byte_intervals, addrs)

    def byte_blocks_in(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[ByteBlock]
        """Finds all the byte blocks that overlap an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_in(self.byte_blocks, addrs)

    def byte_blocks_at(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[ByteBlock]
        """Finds all the byte blocks that begin at an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_at(self.byte_blocks, addrs)

    def code_blocks_in(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[CodeBlock]
        """Finds all the code blocks that overlap an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_in(self.code_blocks, addrs)

    def code_blocks_at(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[CodeBlock]
        """Finds all the code blocks that begin at an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_at(self.code_blocks, addrs)

    def data_blocks_in(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[DataBlock]
        """Finds all the data blocks that overlap an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_in(self.data_blocks, addrs)

    def data_blocks_at(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[DataBlock]
        """Finds all the data blocks that begin at an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_at(self.data_blocks, addrs)
