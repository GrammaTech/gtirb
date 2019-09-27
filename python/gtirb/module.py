from enum import Enum
from uuid import UUID
import CFG_pb2
import Module_pb2
import SymbolicExpression_pb2
import typing

from .auxdata import AuxData, AuxDataContainer
from .block import Block, ProxyBlock
from .dataobject import DataObject
from .imagebytemap import ImageByteMap
from .node import Node
from .section import Section
from .symbol import Symbol
from .symbolicexpression import (
    SymAddrAddr, SymAddrConst, SymStackConst, SymbolicOperand
)
from .util import DictLike


CfgNode = typing.Union[Block, ProxyBlock]
"""A type hint for nodes in the CFG."""


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

        Branch = CFG_pb2.EdgeType.Value('Type_Branch')
        """This edge is the explicit target of a jump instruction.
        May be conditional or unconditional. If conditional, there will be
        a corresponding edge of type :attr:`gtirb.Edge.Type.Fallthrough`.
        """

        Call = CFG_pb2.EdgeType.Value('Type_Call')
        """This edge is the explicit target of a call instruction.
        Unless the function does not return, there will also be a
        corresponding edge of type :attr:`gtirb.Edge.Type.Fallthrough`.
        """

        Fallthrough = CFG_pb2.EdgeType.Value('Type_Fallthrough')
        """This edge represents the lack of control flow; that is,
        two blocks executing in sequence.
        May be conditional, if part of a conditional jump, etc.
        Two blocks with fallthrough edges from one to another
        must be adjacent in memory.
        """

        Return = CFG_pb2.EdgeType.Value('Type_Return')
        """This edge represents a return from a function, generally via a
        return instruction. Since it is not trivial to determine what calls
        calls what functions, return edges may be omitted from valid CFGs.
        """

        Syscall = CFG_pb2.EdgeType.Value('Type_Syscall')
        """This edge is like :class:`gtirb.Edge.Type.Call`, except for
        system call instructions.
        """

        Sysret = CFG_pb2.EdgeType.Value('Type_Sysret')
        """This edge is like :class:`gtirb.Edge.Type.Return`, except for
        system call instructions.
        """

    class Label:
        """Contains a more detailed description of an edge in the CFG.

        :ivar conditional: A boolean indicating if an edge is conditional on
            True or False.
        :ivar direct: A boolean indicating if an edge is direct or indirect.
        :ivar type: The type of the edge.
        """

        def __init__(
            self,
            type,  # type: Edge.Type
            *,
            conditional=False,  # type: bool
            direct=True,  # type: bool
        ):
            """
            :param type: The type of the edge.
            :param conditional: A boolean indicating if an edge is
                conditional on True or False.
            :param direct: A boolean indicating if an edge is
                direct or indirect.
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
            return ("Edge.Label("
                    "type=Edge.{type!s}, "
                    "conditional={conditional!r}, "
                    "direct={direct!r}, "
                    ")".format(**self.__dict__))

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
            if not isinstance(source, (Block, ProxyBlock)):
                raise ValueError(
                    "source UUID %s is not a Block or ProxyBlock" % source_uuid
                )
            if not isinstance(target, (Block, ProxyBlock)):
                raise ValueError(
                    "target UUID %s is not a Block or ProxyBlock" % target_uuid
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
        return ("Edge("
                "source={source!r}, "
                "target={target!r}, "
                "label={label!r}, "
                ")".format(**self.__dict__))


class Module(AuxDataContainer):
    """Represents a loadable object, such as an executable or library.

    :ivar binary_path: The path to the loadable binary object
        represented by this module.
    :ivar blocks: A set of contained Blocks.
    :ivar data: A set of contained DataObjects.
    :ivar image_byte_map: An ImageByteMap containing the raw bytes
        present in the binary.
    :ivar isa_id: The ISA of the binary.
    :ivar file_format: The file format of the binary.
    :ivar name: The name of the binary.
    :ivar preferred_addr: The preferred loading address of the binary.
    :ivar proxies: A set of contained ProxyBlocks.
    :ivar rebase_delta: The rebase delta of the binary.
    :ivar sections: A set of contained Sections.
    :ivar symbols: A set of contained Symbols.
    :ivar symbolic_operands: A dict mapping addresses to symbolic operands
        (i.e., SymAddrAddr, SymAddrConst, or SymStackConst).
    """

    class FileFormat(Enum):
        """Identifies the executable file format of the
       binary represented by a :class:`gtirb.Module`.
        """

        Undefined = Module_pb2.FileFormat.Value('Format_Undefined')
        """An unspecified file format."""

        COFF = Module_pb2.FileFormat.Value('COFF')
        """The Common Object File Format."""

        ELF = Module_pb2.FileFormat.Value('ELF')
        """The Executable and Linkable Format,
        formerly the Extensible Linking Format.
        """

        IdaProDb32 = Module_pb2.FileFormat.Value('IdaProDb32')
        """A 32-bit IDA Pro database file."""

        IdaProDb64 = Module_pb2.FileFormat.Value('IdaProDb64')
        """A 64-bit IDA Pro database file."""

        MACHO = Module_pb2.FileFormat.Value('MACHO')
        """A Mach object file."""

        PE = Module_pb2.FileFormat.Value('PE')
        """Microsoft's Portable Executable format."""

        RAW = Module_pb2.FileFormat.Value('RAW')
        """A raw binary file, with no file format."""

        XCOFF = Module_pb2.FileFormat.Value('XCOFF')
        """The Extended Common Object File Format."""

    class ISAID(Enum):
        """Identifies the instruction set architecture (ISA)
        targeted by a :class:`gtirb.Module`.
        """

        Undefined = Module_pb2.ISAID.Value('ISA_Undefined')
        """An ISA that has not yet been specified.
        This is for unitialized modules;
        use :class:`gtirb.Module.ISAID.ValidButUnsupported`
        instead for specifying undefined ISAs.
        """

        ARM = Module_pb2.ISAID.Value('ARM')
        """The Acorn RISC Machine."""

        IA32 = Module_pb2.ISAID.Value('IA32')
        """The 32-bit Intel Architecture. Also known as i386, x86, or x32."""

        PPC32 = Module_pb2.ISAID.Value('PPC32')
        """IBM's 32-bit PowerPC (Performance Optimization with Enhanced RISC /
        Performance Computing) architecture."""

        X64 = Module_pb2.ISAID.Value('X64')
        """The 64-bit Intel Architecture. Also known as x86_64."""

        ValidButUnsupported = Module_pb2.ISAID.Value('ValidButUnsupported')
        """An unknown or undefined ISA."""

    def __init__(
        self,
        *,
        aux_data=dict(),  # type: DictLike[str, AuxData]
        binary_path='',  # type: str
        blocks=set(),  # type: typing.Iterable[Block]
        cfg=set(),  # type: typing.Iterable[Edge]
        data=set(),  # type: typing.Iterable[DataObject]
        file_format=FileFormat.Undefined,  # type: Module.FileFormat
        image_byte_map=None,  # type: typing.Optional[ImageByteMap]
        isa_id=ISAID.Undefined,  # type: Module.ISAID
        name='',  # type: str
        preferred_addr=0,  # type: int
        proxies=set(),  # type: typing.Iterable[ProxyBlock]
        rebase_delta=0,  # type: int
        sections=set(),  # type: typing.Iterable[Section]
        symbols=set(),  # type: typing.Iterable[Symbol]
        symbolic_operands=dict(),  # type: DictLike[int, SymbolicOperand]
        uuid=None,  # type: typing.Optional[UUID]
    ):
        # type: (...) -> None
        """
        :param aux_data: The initial auxiliary data to be associated
            with the object, as a mapping from names to
            :class:`gtirb.AuxData`, defaults to an empty :class:`dict`.
        :param binary_path: The path to the binary.
        :param blocks: A set of contained Blocks.
        :param cfg: The value of :attr:`self.cfg`.
        :param data: A set of contained DataObjects.
        :param file_format: The file format of the binary.
        :param image_byte_map: An ImageByteMap containing the raw bytes
            present in the binary.
        :param isa_id: The ISA of the binary.
        :param name: The name of the binary.
        :param preferred_addr: The preferred loading address of the binary.
        :param proxies: A set of contained ProxyBlocks.
        :param rebase_delta: The rebase delta of the binary.
        :param sections: A set of contained Sections.
        :param symbols: A set of contained Symbols.
        :param symbolic_operands: A dict mapping addresses to symbolic operands
            (i.e., SymAddrAddr, SymAddrConst, or SymStackConst).
        :param uuid: The UUID of this Node,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        """

        if image_byte_map is None:
            image_byte_map = ImageByteMap()

        self.binary_path = binary_path  # type: str
        self.blocks = set(blocks)  # type: typing.Set[Block]
        self.data = set(data)  # type: typing.Set[DataObject]
        self.image_byte_map = image_byte_map  # type: ImageByteMap
        self.isa_id = isa_id  # type: "Module.ISAID"
        self.file_format = file_format  # type: "Module.FileFormat"
        self.name = name  # type: str
        self.preferred_addr = preferred_addr  # type: int
        self.proxies = set(proxies)  # type: typing.Set[ProxyBlock]
        self.rebase_delta = rebase_delta  # type: int
        self.sections = set(sections)  # type: typing.Set[Section]
        self.symbols = set(symbols)  # type: typing.Set[Symbol]
        self.symbolic_operands = dict(
            symbolic_operands
        )  # type: typing.Dict[int, SymbolicOperand]

        # Initialize the CFG and aux data last so that the cache is populated
        super().__init__(aux_data, uuid)
        self.cfg = set(cfg)  # type: typing.Set[Edge]

    @classmethod
    def _decode_protobuf(cls, proto_module, uuid):
        # type: (Module_pb2.Module. UUID) -> Module
        aux_data = (
            (k, AuxData._from_protobuf(v))
            for k, v in proto_module.aux_data_container.aux_data.items()
        )
        blocks = (Block._from_protobuf(b) for b in proto_module.blocks)
        cfg = (Edge._from_protobuf(e) for e in proto_module.cfg.edges)
        data = (DataObject._from_protobuf(d) for d in proto_module.data)
        proxies = (ProxyBlock._from_protobuf(p) for p in proto_module.proxies)
        ibm = ImageByteMap._from_protobuf(proto_module.image_byte_map)
        sections = (Section._from_protobuf(s) for s in proto_module.sections)
        symbols = (Symbol._from_protobuf(s) for s in proto_module.symbols)

        def sym_expr_from_protobuf(symbolic_expression):
            if symbolic_expression.HasField("stack_const"):
                return SymStackConst._from_protobuf(
                    symbolic_expression.stack_const
                )
            if symbolic_expression.HasField("addr_const"):
                return SymAddrConst._from_protobuf(
                    symbolic_expression.addr_const
                )
            if symbolic_expression.HasField("addr_addr"):
                return SymAddrAddr._from_protobuf(
                    symbolic_expression.addr_addr
                )

        symbolic_operands = (
            (k, sym_expr_from_protobuf(v))
            for k, v in proto_module.symbolic_operands.items()
        )

        return cls(
            aux_data=aux_data,
            binary_path=proto_module.binary_path,
            blocks=blocks,
            cfg=cfg,
            data=data,
            image_byte_map=ibm,
            isa_id=Module.ISAID(proto_module.isa_id),
            file_format=Module.FileFormat(proto_module.file_format),
            name=proto_module.name,
            preferred_addr=proto_module.preferred_addr,
            proxies=proxies,
            rebase_delta=proto_module.rebase_delta,
            sections=sections,
            symbols=symbols,
            symbolic_operands=symbolic_operands,
            uuid=uuid,
        )

    def _to_protobuf(self):
        # type: () -> Module_pb2.Module
        proto_module = Module_pb2.Module()
        proto_module.aux_data_container.CopyFrom(super()._to_protobuf())
        proto_module.binary_path = self.binary_path
        proto_module.blocks.extend(b._to_protobuf() for b in self.blocks)
        proto_cfg = CFG_pb2.CFG()
        proto_cfg.vertices.extend(
            v.uuid.bytes for v in self.blocks | self.proxies
        )
        proto_cfg.edges.extend(e._to_protobuf() for e in self.cfg)
        proto_module.cfg.CopyFrom(proto_cfg)
        proto_module.data.extend(d._to_protobuf() for d in self.data)
        proto_module.image_byte_map.CopyFrom(
            self.image_byte_map._to_protobuf()
        )
        proto_module.isa_id = self.isa_id.value
        proto_module.file_format = self.file_format.value
        proto_module.name = self.name
        proto_module.preferred_addr = self.preferred_addr
        proto_module.proxies.extend(p._to_protobuf() for p in self.proxies)
        proto_module.rebase_delta = self.rebase_delta
        proto_module.sections.extend(s._to_protobuf() for s in self.sections)
        proto_module.symbols.extend(s._to_protobuf() for s in self.symbols)
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
                    "Expected SymStackConst, SymAddrAddr or SymAddrConst"
                )
            proto_module.symbolic_operands[k].CopyFrom(sym_exp)
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
            "isa_id",
            "file_format",
            "name",
            "preferred_addr",
            "rebase_delta",
            "symbolic_operands",
        ):
            if getattr(self, attr) != getattr(other, attr):
                return False

        for attr in ("blocks", "data", "proxies", "sections", "symbols"):
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

        return self.image_byte_map.deep_eq(other.image_byte_map)

    def __repr__(self):
        # type: () -> str
        return ("Module("
                "uuid={uuid!r}, "
                "name={name!r}, "
                "binary_path={binary_path!r}, "
                "isa_id=Module.{isa_id!s}, "
                "file_format=Module.{file_format!s}, "
                "preferred_addr={preferred_addr:#x}, "
                "rebase_delta={rebase_delta:#x}, "
                "blocks={blocks!r}, "
                "data={data!r}, "
                "image_byte_map={image_byte_map!r}, "
                "proxies={proxies!r}, "
                "sections={sections!r}, "
                "symbols={symbols!r}, "
                "symbolic_operands={symbolic_operands!r}, "
                ")".format(**self.__dict__))
