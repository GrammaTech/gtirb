from enum import Enum
from uuid import UUID

import CFG_pb2
import Module_pb2
import SymbolicExpression_pb2

from .auxdata import AuxData, AuxDataContainer
from .block import Block, ProxyBlock
from .dataobject import DataObject
from .imagebytemap import ImageByteMap
from .node import Node
from .section import Section
from .symbol import Symbol
from .symbolicexpression import SymAddrAddr, SymAddrConst, SymStackConst


class Edge:
    """An Edge in the CFG.

    Parameters:
        label: an EdgeLabel for this edge
        source: source Block
        target: target Block

    """
    def __init__(self, label, source, target):
        self.label = label
        self.source = source
        self.target = target

    @classmethod
    def _from_protobuf(cls, edge):
        source_uuid = UUID(bytes=edge.source_uuid)
        target_uuid = UUID(bytes=edge.target_uuid)
        try:
            source = Node._uuid_cache[source_uuid]
            target = Node._uuid_cache[target_uuid]
            if not isinstance(source, (Block, ProxyBlock)):
                raise ValueError("source UUID %s is not a Block or ProxyBlock"
                                 % source_uuid)
            if not isinstance(target, (Block, ProxyBlock)):
                raise ValueError("target UUID %s is not a Block or ProxyBlock"
                                 % target_uuid)
        except KeyError as e:
            raise KeyError("Could not find UUID %s when creating edge %s -> %s"
                           % (e, source_uuid, target_uuid))
        return cls(EdgeLabel._from_protobuf(edge.label), source, target)

    def _to_protobuf(self):
        proto_edge = CFG_pb2.Edge()
        proto_edge.source_uuid = self.source.uuid.bytes
        proto_edge.target_uuid = self.target.uuid.bytes
        proto_edge.label.CopyFrom(self.label._to_protobuf())
        return proto_edge

    def __eq__(self, other):
        if not isinstance(other, Edge):
            return False
        return self.label == other.label \
            and self.source.uuid == other.source.uuid \
            and self.target.uuid == other.target.uuid

    def __hash__(self):
        return hash((self.label, self.source.uuid, self.target.uuid))


class EdgeLabel:
    """A label for an Edge

    Attributes:
        conditional: boolean indicating if an edge is conditional on
            True or False
        direct: boolean indicating if an edge is direct or indirect
        type: EdgeType of the edge

    """
    class EdgeType(Enum):
        """Type of control flow transfer indicated by this edge"""

        Branch = CFG_pb2.EdgeType.Value('Type_Branch')
        Call = CFG_pb2.EdgeType.Value('Type_Call')
        Fallthrough = CFG_pb2.EdgeType.Value('Type_Fallthrough')
        Return = CFG_pb2.EdgeType.Value('Type_Return')
        Syscall = CFG_pb2.EdgeType.Value('Type_Syscall')
        Sysret = CFG_pb2.EdgeType.Value('Type_Sysret')

    def __init__(self, conditional, direct, type):
        self.conditional = conditional
        self.direct = direct
        self.type = type

    @classmethod
    def _from_protobuf(cls, edge_label):
        return cls(edge_label.conditional, edge_label.direct,
                   EdgeLabel.EdgeType(edge_label.type))

    def _to_protobuf(self):
        proto_edgelabel = CFG_pb2.EdgeLabel()
        proto_edgelabel.conditional = self.conditional
        proto_edgelabel.direct = self.direct
        proto_edgelabel.type = self.type.value
        return proto_edgelabel

    def __eq__(self, other):
        if not isinstance(other, EdgeLabel):
            return False
        return self.conditional == other.conditional \
            and self.direct == other.direct \
            and self.type == other.type

    def __hash__(self):
        return hash((self.conditional, self.direct, self.type))


class Module(AuxDataContainer):
    """Represents loadable objects such as executables or libraries

    Attributes:
        binary_path: the path to the binary
        blocks: set of contained Blocks
        data: set of contained DataObjects
        image_byte_map: ImageByteMap containing the raw data in the binary
        isa_id: ISAID of the binary
        file_format: FileFormat of the binary
        name: name of the binary
        preferred_addr: preferred loading address of the binary
        proxies: set of contained ProxyBlocks
        rebase_delta: rebase delta of the binary
        sections: set of contained Sections
        symbols: set of contained Symbols
        symbolic_operands: dict mapping addresses to symbolic operands.
            (i.e., SymAddrAddr, SymAddrConst, SymStackConst)
        uuid: the UUID of this Node

    """
    class FileFormat(Enum):
        """Identifies an executable file format

        Undefined: uninitialized
        COFF: Common Object File Format
        ELF: Executable and Linkable Format
            (formerly Extensible Linking Format)
        IdaProDb32: 32-bit IDA Pro database file
        IdaProDb64: 64-bit IDA Pro database file
        MACHO: Mach object file
        PE: Microsoft Portable Executable
        RAW: Raw binary file (no format)
        XCOFF: Non-COFF (files start with ANON_OBJECT_HEADER*)

        """
        Undefined = Module_pb2.FileFormat.Value('Format_Undefined')
        COFF = Module_pb2.FileFormat.Value('COFF')
        ELF = Module_pb2.FileFormat.Value('ELF')
        IdaProDb32 = Module_pb2.FileFormat.Value('IdaProDb32')
        IdaProDb64 = Module_pb2.FileFormat.Value('IdaProDb64')
        MACHO = Module_pb2.FileFormat.Value('MACHO')
        PE = Module_pb2.FileFormat.Value('PE')
        RAW = Module_pb2.FileFormat.Value('RAW')
        XCOFF = Module_pb2.FileFormat.Value('XCOFF')

    class ISAID(Enum):
        """Identifies an instruction set architecture (ISA)

        Undefined: uninitialized
        ARM: Advanced/Acorn RISC Machine
        IA32: Intel Architecture, 32-bit. Also known as i386
        PPC32: Performance Optimization with Enhanced RISC -
            Performance Computing, 32-bit
        X64: 64-bit extensions to Intel/AMD x86 ISA
        ValidButUnsupported: Valid, but unsupported ISA

        """
        Undefined = Module_pb2.ISAID.Value('ISA_Undefined')
        ARM = Module_pb2.ISAID.Value('ARM')
        IA32 = Module_pb2.ISAID.Value('IA32')
        PPC32 = Module_pb2.ISAID.Value('PPC32')
        X64 = Module_pb2.ISAID.Value('X64')
        ValidButUnsupported = Module_pb2.ISAID.Value('ValidButUnsupported')

    def __init__(self,
                 *,
                 aux_data=dict(),
                 binary_path='',
                 blocks=set(),
                 cfg=set(),
                 data=set(),
                 file_format=FileFormat.Undefined,
                 image_byte_map=None,
                 isa_id=ISAID.Undefined,
                 name='',
                 preferred_addr=0,
                 proxies=set(),
                 rebase_delta=0,
                 sections=set(),
                 symbols=set(),
                 symbolic_operands=dict(),
                 uuid=None):
        if image_byte_map is None:
            image_byte_map = ImageByteMap()

        self.binary_path = binary_path
        self.blocks = set(blocks)
        self.data = set(data)
        self.image_byte_map = image_byte_map
        self.isa_id = isa_id
        self.file_format = file_format
        self.name = name
        self.preferred_addr = preferred_addr
        self.proxies = set(proxies)
        self.rebase_delta = rebase_delta
        self.sections = set(sections)
        self.symbols = set(symbols)
        self.symbolic_operands = dict(symbolic_operands)

        # Initialize the CFG and aux data last so that the cache is populated
        super().__init__(aux_data, uuid)
        self.cfg = set(cfg)

    @classmethod
    def _decode_protobuf(cls, proto_module, uuid):
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
            if symbolic_expression.HasField('stack_const'):
                return SymStackConst._from_protobuf(
                    symbolic_expression.stack_const)
            if symbolic_expression.HasField('addr_const'):
                return SymAddrConst._from_protobuf(
                    symbolic_expression.addr_const)
            if symbolic_expression.HasField('addr_addr'):
                return SymAddrAddr._from_protobuf(
                    symbolic_expression.addr_addr)
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
            uuid=uuid
        )

    def _to_protobuf(self):
        proto_module = Module_pb2.Module()
        proto_module.aux_data_container.CopyFrom(super()._to_protobuf())
        proto_module.binary_path = self.binary_path
        proto_module.blocks.extend(b._to_protobuf() for b in self.blocks)
        proto_cfg = CFG_pb2.CFG()
        proto_cfg.vertices.extend(
            v.uuid.bytes for v in self.blocks | self.proxies)
        proto_cfg.edges.extend(e._to_protobuf() for e in self.cfg)
        proto_module.cfg.CopyFrom(proto_cfg)
        proto_module.data.extend(d._to_protobuf() for d in self.data)
        proto_module.image_byte_map.CopyFrom(
            self.image_byte_map._to_protobuf())
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
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not super().deep_eq(other):
            return False
        if not isinstance(other, Module):
            return False
        for attr in ('binary_path', 'isa_id', 'file_format', 'name',
                     'preferred_addr', 'rebase_delta', 'symbolic_operands'):
            if getattr(self, attr) != getattr(other, attr):
                return False

        for attr in ('blocks', 'data', 'proxies', 'sections', 'symbols'):
            self_nodes = sorted(getattr(self, attr), key=lambda n: n.uuid)
            other_nodes = sorted(getattr(other, attr), key=lambda n: n.uuid)
            if not len(self_nodes) == len(other_nodes):
                return False
            for self_node, other_node in zip(self_nodes, other_nodes):
                if not self_node.deep_eq(other_node):
                    return False

        self_edges = sorted(self.cfg,
                            key=lambda e: (e.source.uuid, e.target.uuid))
        other_edges = sorted(other.cfg,
                             key=lambda e: (e.source.uuid, e.target.uuid))
        if not len(self_edges) == len(other_edges):
            return False
        for self_edge, other_edge in zip(self_edges, other_edges):
            if self_edge != other_edge:
                return False

        return self.image_byte_map.deep_eq(other.image_byte_map)
