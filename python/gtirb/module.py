from enum import Enum
from uuid import UUID
import CFG_pb2
import Module_pb2
import typing
import itertools

from .auxdata import AuxData, AuxDataContainer
from .block import CodeBlock, DataBlock, ProxyBlock, CfgNode, ByteBlock
from .byteinterval import ByteInterval
from .node import Node
from .section import Section
from .symbol import Symbol
from .util import DictLike, SetWrapper


class Edge:
    """An edge in the CFG from ``source`` to ``target``, with optional
    control-flow details in ``label``.

    :ivar source: The source CFG node.
    :ivar target: The target CFG node.
    :ivar label: An optional label containing more control flow information.
    """

    class Type(Enum):
        """The type of control flow transfer indicated by a
        :class:`gtirb.Edge`.
        """

        Branch = CFG_pb2.EdgeType.Value("Type_Branch")
        """This edge is the explicit target of a jump instruction.
        May be conditional or unconditional. If conditional, there will be
        a corresponding edge of type :attr:`gtirb.Edge.Type.Fallthrough`.
        """

        Call = CFG_pb2.EdgeType.Value("Type_Call")
        """This edge is the explicit target of a call instruction.
        Unless the function does not return, there will also be a
        corresponding edge of type :attr:`gtirb.Edge.Type.Fallthrough`.
        """

        Fallthrough = CFG_pb2.EdgeType.Value("Type_Fallthrough")
        """This edge represents two blocks executing in sequence.
        This occurs on the non-branching paths of conditional branch
        instructions, after call instructons have returned, and when two
        blocks have no control flow between them, but another
        :class:`gtirb.Edge` targets the target block.
        If there exists a fallthrough edge from block ``A`` to block ``B``,
        then ``A`` must immediately precede ``B`` in memory.
        """

        Return = CFG_pb2.EdgeType.Value("Type_Return")
        """This edge represents a return from a function, generally via a
        return instruction. Return edges may be omitted from valid CFGs;
        a function may have an uncomputable number of possible return sites,
        due to the possibility of indirect calls.
        """

        Syscall = CFG_pb2.EdgeType.Value("Type_Syscall")
        """This edge is the explicit target of a system call instruction.
        Unless the function does not return, there will also be a
        corresponding edge of type :attr:`gtirb.Edge.Type.Fallthrough`. This
        is the system call equivalent to :class:`gtirb.Edge.Type.Call`.
        """

        Sysret = CFG_pb2.EdgeType.Value("Type_Sysret")
        """This edge represents a return from a system call, generally via a
        return instruction. Return edges may be omitted from valid CFGs;
        a function may have an uncomputable number of possible return sites,
        due to the possibility of indirect calls. This is the system call
        equivalent to :class:`gtirb.Edge.Type.Return`.
        """

    class Label:
        """Contains a more detailed description of a :class:`gtirb.Edge`
        in the CFG.

        :ivar conditional: When this edge is part of a conditional branch,
            ``conditional`` is ``True`` when the edge represents the control
            flow taken when the branch's condition is met, and ``False``
            when it represents the control flow taken when the branch's
            condition is not met. Otherwise, it is always ``False``.
        :ivar direct: ``True`` if the branch or call is direct,
                and ``False`` if it is indirect. If an edge is indirect,
                then all outgoing indirect edges represent the set of
                possible locations the edge may branch to. If there
                exists an indirect outgoing edge to a :class:`gtirb.ProxyBlock`
                without any :class:`gtirb.Symbol` objects referring to it,
                then the set of all possible branch locations is unknown.
        :ivar type: The type of control flow the :class:`gtirb.Edge`
            represents.
        """

        def __init__(
            self,
            type,  # type: Edge.Type
            *,
            conditional=False,  # type: bool
            direct=True  # type: bool
        ):
            """
            :param type: The type of control flow the :class:`gtirb.Edge`
                represents.
            :param conditional: ``True`` when this edge represents a
                conditional branch taken when the branch's condition is met,
                and ``False`` otherwise.
            :param direct: ``True`` if the branch or call is direct,
                and ``False`` if it is indirect.
            """

            self.type = type  # type: Edge.Type
            self.conditional = conditional  # type: bool
            self.direct = direct  # type: bool

        @classmethod
        def _from_protobuf(cls, label):
            # type: (CFG_pb2.EdgeLabel) -> Edge.Label
            return Edge.Label(
                type=Edge.Type(label.type),
                conditional=label.conditional,
                direct=label.direct,
            )

        def _to_protobuf(self):
            # type: () -> CFG_pb2.EdgeLabel
            proto_label = CFG_pb2.EdgeLabel()
            proto_label.type = self.type.value
            proto_label.conditional = self.conditional
            proto_label.direct = self.direct
            return proto_label

        def __eq__(self, other):
            # type: (typing.Any) -> bool
            if not isinstance(other, Edge.Label):
                return False
            return (
                self.type == other.type
                and self.conditional == other.conditional
                and self.direct == other.direct
            )

        def __hash__(self):
            # type: () -> int
            return hash((self.type, self.conditional, self.direct))

        def __repr__(self):
            # type: () -> str
            return (
                "Edge.Label("
                "type=Edge.{type!s}, "
                "conditional={conditional!r}, "
                "direct={direct!r}, "
                ")".format(**self.__dict__)
            )

    def __init__(
        self,
        source,  # type: CfgNode
        target,  # type: CfgNode
        label=None,  # type: typing.Optional["Edge.Label"]
    ):
        # type: (...) -> None
        """
        :param source: The source CFG node.
        :param target: The target CFG node.
        :param label: An optional label
            containing more control flow information.
        """
        self.source = source  # type: CfgNode
        self.target = target  # type: CfgNode
        self.label = label  # type: typing.Optional["Edge.Label"]

    @classmethod
    def _from_protobuf(cls, edge):
        # type: (CFG_pb2.Edge) -> Edge
        source_uuid = UUID(bytes=edge.source_uuid)
        target_uuid = UUID(bytes=edge.target_uuid)
        try:
            source = Node._uuid_cache[source_uuid]
            target = Node._uuid_cache[target_uuid]
            if not isinstance(source, CfgNode):
                raise ValueError(
                    "source UUID %s is not a CfgNode" % source_uuid
                )
            if not isinstance(target, (CodeBlock, ProxyBlock)):
                raise ValueError(
                    "target UUID %s is not a CfgNode" % target_uuid
                )
        except KeyError as e:
            raise KeyError(
                "Could not find UUID %s when creating edge %s -> %s"
                % (e, source_uuid, target_uuid)
            )
        label = None
        if edge.label is not None:
            label = Edge.Label._from_protobuf(edge.label)
        return cls(source, target, label)

    def _to_protobuf(self):
        # type: () -> CFG_pb2.Edge
        proto_edge = CFG_pb2.Edge()
        proto_edge.source_uuid = self.source.uuid.bytes
        proto_edge.target_uuid = self.target.uuid.bytes
        if self.label is not None:
            proto_edge.label.CopyFrom(self.label._to_protobuf())
        return proto_edge

    def __eq__(self, other):
        # type: (typing.Any) -> bool
        if not isinstance(other, Edge):
            return False
        return (
            self.source.uuid == other.source.uuid
            and self.target.uuid == other.target.uuid
            and self.label == other.label
        )

    def __hash__(self):
        # type: () -> int
        return hash((self.source.uuid, self.target.uuid, self.label))

    def __repr__(self):
        # type: () -> str
        return (
            "Edge("
            "source={source!r}, "
            "target={target!r}, "
            "label={label!r}, "
            ")".format(**self.__dict__)
        )


class Module(AuxDataContainer):
    """Represents a loadable object, such as an executable or library.

    :ivar binary_path: The path to the loadable binary object
        represented by this module. An empty string if not specified.
        The file represented by this path is indicitave of what file
        this ``Module`` was initially created from; it is not guaranteed to
        currently exist or have the same contents.
    :ivar isa: The ISA of the binary.
    :ivar file_format: The file format of the binary.
    :ivar name: The name given to the binary. Some file formats use this
        for linking and/or symbol resolution purposes. An empty string if
        not specified by the format.
    :ivar preferred_addr: The preferred loading address of the binary.
    :ivar proxies: A set containing all the :class:`gtirb.ProxyBlock`\\s
        in the binary.
    :ivar rebase_delta: The rebase delta of the binary.
    :ivar sections: A set containing all the :class:`gtirb.Section`\\s
        in the binary.
    :ivar symbols: A set containing all the :class:`gtirb.Symbol`\\s
        in the binary.
    """

    class FileFormat(Enum):
        """Identifies the executable file format of the binary represented
        by a :class:`gtirb.Module`.
        """

        Undefined = Module_pb2.FileFormat.Value("Format_Undefined")
        """A file format that has not yet been specified.
        This is for unitialized modules; do not use to refer to
        file formats without ``FileFormat`` values.
        """

        COFF = Module_pb2.FileFormat.Value("COFF")
        """The Common Object File Format."""

        ELF = Module_pb2.FileFormat.Value("ELF")
        """The Executable and Linkable Format,
        formerly the Extensible Linking Format.
        """

        IdaProDb32 = Module_pb2.FileFormat.Value("IdaProDb32")
        """A 32-bit IDA Pro database file."""

        IdaProDb64 = Module_pb2.FileFormat.Value("IdaProDb64")
        """A 64-bit IDA Pro database file."""

        MACHO = Module_pb2.FileFormat.Value("MACHO")
        """A Mach object file."""

        PE = Module_pb2.FileFormat.Value("PE")
        """Microsoft's Portable Executable format."""

        RAW = Module_pb2.FileFormat.Value("RAW")
        """A raw binary file, with no file format."""

        XCOFF = Module_pb2.FileFormat.Value("XCOFF")
        """The Extended Common Object File Format."""

    class ISA(Enum):
        """Identifies the instruction set architecture (ISA)
        targeted by a :class:`gtirb.Module`.
        """

        Undefined = Module_pb2.ISA.Value("ISA_Undefined")
        """An ISA that has not yet been specified.
        This is for unitialized modules;
        use :class:`gtirb.Module.ISA.ValidButUnsupported`
        instead for specifying undefined ISAs.
        """

        ARM = Module_pb2.ISA.Value("ARM")
        """The Acorn RISC Machine."""

        IA32 = Module_pb2.ISA.Value("IA32")
        """The 32-bit Intel Architecture. Also known as i386, x86, or x32."""

        PPC32 = Module_pb2.ISA.Value("PPC32")
        """IBM's 32-bit PowerPC (Performance Optimization with Enhanced RISC /
        Performance Computing) architecture."""

        X64 = Module_pb2.ISA.Value("X64")
        """The 64-bit Intel Architecture. Also known as x86_64."""

        ValidButUnsupported = Module_pb2.ISA.Value("ValidButUnsupported")
        """An unknown or undefined ISA."""

    class _NodeSet(SetWrapper):
        def __init__(self, field):
            # type: (str) -> None
            self._field = field  # type: str

        def add(self, v):
            if v._module is not None:
                getattr(v._module, self._field).discard(v)
            v._module = self
            return super().add(v)

        def discard(self, v):
            v._module = None
            return super().discard(v)

    def __init__(
        self,
        *,
        aux_data=dict(),  # type: DictLike[str, AuxData]
        binary_path="",  # type: str
        cfg=set(),  # type: typing.Iterable[Edge]
        file_format=FileFormat.Undefined,  # type: Module.FileFormat
        isa=ISA.Undefined,  # type: Module.ISA
        name="",  # type: str
        preferred_addr=0,  # type: int
        proxies=set(),  # type: typing.Iterable[ProxyBlock]
        rebase_delta=0,  # type: int
        sections=set(),  # type: typing.Iterable[Section]
        symbols=set(),  # type: typing.Iterable[Symbol]
        uuid=None  # type: typing.Optional[UUID]
    ):
        # type: (...) -> None
        """
        :param aux_data: The initial auxiliary data to be associated
            with the object, as a mapping from names to
            :class:`gtirb.AuxData`, defaults to an empty :class:`dict`.
        :param binary_path: The path to the loadable binary object
            represented by this module.
        :param isa: The ISA of the binary.
        :param file_format: The file format of the binary.
        :param name: The name given to the binary.
        :param preferred_addr: The preferred loading address of the binary.
        :param proxies: A set containing all the :class:`gtirb.ProxyBlock`\\s
            in the binary.
        :param rebase_delta: The rebase delta of the binary.
        :param sections: A set containing all the :class:`gtirb.Section`\\s
            in the binary.
        :param symbols: A set containing all the :class:`gtirb.Symbol`\\s
            in the binary.
        :param uuid: The UUID of this ``Module``,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        """

        self.binary_path = binary_path  # type: str
        self.isa = isa  # type: Module.ISA
        self.file_format = file_format  # type: Module.FileFormat
        self.name = name  # type: str
        self.preferred_addr = preferred_addr  # type: int
        self.proxies = Module._NodeSet(
            proxies, "proxies"
        )  # type: typing.Set[ProxyBlock]
        self.rebase_delta = rebase_delta  # type: int
        self.sections = Module._NodeSet(
            sections, "sections"
        )  # type: typing.Set[Section]
        self.symbols = Module._NodeSet(
            symbols, "symbols"
        )  # type: typing.Set[Symbol]
        self._ir = None  # type: "IR"
        # Initialize the CFG and aux data last so that the cache is populated
        super().__init__(aux_data, uuid)
        self.cfg = set(cfg)  # type: typing.Set[Edge]

    @classmethod
    def _decode_protobuf(cls, proto_module, uuid):
        # type: (Module_pb2.Module. UUID) -> Module

        # proxies depend on nothing
        proxies = [ProxyBlock._from_protobuf(p) for p in proto_module.proxies]
        # sections depend on symbolic expressions, so that step is split out
        # from _decode_protobuf into _decode_symbolic_expressions
        sections = [Section._from_protobuf(s) for s in proto_module.sections]
        # CFG depends on cfg nodes
        cfg = [Edge._from_protobuf(e) for e in proto_module.cfg.edges]
        # symbols depend on blocks
        symbols = [Symbol._from_protobuf(s) for s in proto_module.symbols]
        # symbolic expressions depend on symbols
        for section in sections:
            for interval in section.byte_intervals:
                interval._decode_symbolic_expressions()
        # aux data may depend on any node
        aux_data = AuxDataContainer._read_protobuf_aux_data(proto_module)

        return cls(
            aux_data=aux_data,
            binary_path=proto_module.binary_path,
            cfg=cfg,
            isa=Module.ISA(proto_module.isa),
            file_format=Module.FileFormat(proto_module.file_format),
            name=proto_module.name,
            preferred_addr=proto_module.preferred_addr,
            proxies=proxies,
            rebase_delta=proto_module.rebase_delta,
            sections=sections,
            symbols=symbols,
            uuid=uuid,
        )

    def _to_protobuf(self):
        # type: () -> Module_pb2.Module
        proto_module = Module_pb2.Module()
        self._write_protobuf_aux_data(proto_module)
        proto_module.binary_path = self.binary_path
        proto_cfg = CFG_pb2.CFG()
        proto_cfg.vertices.extend(v.uuid.bytes for v in self.cfg_nodes)
        proto_cfg.edges.extend(e._to_protobuf() for e in self.cfg)
        proto_module.cfg.CopyFrom(proto_cfg)
        proto_module.isa = self.isa.value
        proto_module.file_format = self.file_format.value
        proto_module.name = self.name
        proto_module.preferred_addr = self.preferred_addr
        proto_module.proxies.extend(p._to_protobuf() for p in self.proxies)
        proto_module.rebase_delta = self.rebase_delta
        proto_module.sections.extend(s._to_protobuf() for s in self.sections)
        proto_module.symbols.extend(s._to_protobuf() for s in self.symbols)
        proto_module.uuid = self.uuid.bytes
        return proto_module

    def deep_eq(self, other):
        # type: (typing.Any) -> bool
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not super().deep_eq(other):
            return False
        if not isinstance(other, Module):
            return False
        for attr in (
            "binary_path",
            "isa",
            "file_format",
            "name",
            "preferred_addr",
            "rebase_delta",
        ):
            if getattr(self, attr) != getattr(other, attr):
                return False

        for attr in ("proxies", "sections", "symbols"):
            self_nodes = sorted(getattr(self, attr), key=lambda n: n.uuid)
            other_nodes = sorted(getattr(other, attr), key=lambda n: n.uuid)
            if not len(self_nodes) == len(other_nodes):
                return False
            for self_node, other_node in zip(self_nodes, other_nodes):
                if not self_node.deep_eq(other_node):
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
            if self_edge != other_edge:
                return False

        return True

    def __repr__(self):
        # type: () -> str
        return (
            "Module("
            "uuid={uuid!r}, "
            "name={name!r}, "
            "binary_path={binary_path!r}, "
            "isa=Module.{isa!s}, "
            "file_format=Module.{file_format!s}, "
            "preferred_addr={preferred_addr:#x}, "
            "rebase_delta={rebase_delta:#x}, "
            "proxies={proxies!r}, "
            "sections={sections!r}, "
            "symbols={symbols!r}, "
            ")".format(**self.__dict__)
        )

    @property
    def ir(self):
        # type: () -> typing.Optional["IR"]
        """The :class:`IR` this module belongs to."""

        return self._ir

    @ir.setter
    def ir(self, value):
        # type: (typing.Optional["IR"]) -> None
        if self._ir is not None:
            self._ir.modules.remove(self)
        if value is not None:
            value.modules.append(self)

    @property
    def byte_intervals(self):
        # type: () -> typing.Iterator[ByteInterval]
        """The :class:`ByteInterval`\\s in this module."""

        return itertools.chain.from_iterable(
            s.byte_intervals for s in self.sections
        )

    @property
    def byte_blocks(self):
        # type: () -> typing.Iterator[ByteBlock]
        """The :class:`ByteBlock`\\s in this module."""

        return itertools.chain.from_iterable(
            s.byte_blocks for s in self.sections
        )

    @property
    def code_blocks(self):
        # type: () -> typing.Iterator[CodeBlock]
        """The :class:`CodeBlock`\\s in this module."""

        return itertools.chain.from_iterable(
            s.code_blocks for s in self.sections
        )

    @property
    def data_blocks(self):
        # type: () -> typing.Iterator[DataBlock]
        """The :class:`DataBlock`\\s in this module."""

        return itertools.chain.from_iterable(
            s.data_blocks for s in self.sections
        )

    @property
    def cfg_nodes(self):
        # type: () -> typing.Iterator[CfgNode]
        """The :class:`CfgNode`\\s in this module."""

        return itertools.chain(self.code_blocks, self.proxies)
