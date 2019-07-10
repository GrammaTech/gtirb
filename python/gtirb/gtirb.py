# -*- coding: utf-8 -*-
"""
The GTIRB python module.

Provides a python API into GTIRB, allowing you to open GTIR protobuf
files, manipulating them and writing them back.

Sample usage.

Opening a GTIR file and loading it into an IR instance

    ir = IR.load_protobuf('filename.gtir')

Writing back the ir instance as a protobuf file

    IR.save_protobuf('filename.gtir')

TODOS:
* Implement __repr__ functions for all the classes.
* Support creating default instances for all classes.

"""
import io
import os
import sys

from uuid import UUID, uuid4
from enum import Enum

dir_path = os.path.dirname(os.path.realpath(__file__))
sys.path.append(dir_path)

import AuxData_pb2
import AuxDataContainer_pb2
import Block_pb2
import ByteMap_pb2
import CFG_pb2
import DataObject_pb2
import ImageByteMap_pb2
import IR_pb2
import Module_pb2
import Offset_pb2
import ProxyBlock_pb2
import Section_pb2
import Symbol_pb2
import SymbolicExpression_pb2

import serialization
# The global serializer instance. User can use this to register new
# encoders/decoders.
serializer = serialization.Serialization()


class AuxData:
    """Types and operations for auxiliary data.  AuxData objects can be
    attached to the IR or individual Modules to store additional
    client-specific data in a portable way.
    """

    def __init__(self, data, type_name=None):
        """Constructor
        :param data: aux data
        :param type_name: `str`, optional. Type of the data.
        """
        self.data = data
        self.type_name = type_name

    def _to_protobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = AuxData_pb2.AuxData()
        out_bytes_array = io.BytesIO()
        check_type_name = serializer.encode(out_bytes_array, self.data,
                                            type_name_hint=self.type_name)
        ret.type_name = check_type_name
        out_bytes_array.seek(0)
        ret.data = out_bytes_array.read()
        return ret

    @classmethod
    def _from_protobuf(cls, aux_data, uuid_cache=None):
        """
        Load pygtirb class from protobuf class
        """
        ret = serializer.decode(aux_data.type_name, io.BytesIO(aux_data.data))
        return cls(data=ret, type_name=aux_data.type_name)


class AuxDataContainer:
    """Holds AuxData tables, base class for IR and Module
    """

    def __init__(self, aux_data=None):
        """Constructor
        :param aux_data: dict(str, AuxData), optional dict mapping
            type names to AuxData objects
        :returns: AuxDataContainer
        :rtype: AuxDataContainer
        """
        if aux_data is None:
            self.aux_data = dict()
        else:
            self.aux_data = dict(aux_data)

    def _to_protobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = AuxDataContainer_pb2.AuxDataContainer()
        for k, v in self.aux_data.items():
            ret.aux_data[k].CopyFrom(v._to_protobuf())
        return ret

    @classmethod
    def _from_protobuf(cls, aux_data_container, uuid_cache=None):
        """Load pygtirb object from protobuf object

        :param cls: this class
        :param aux_data_container: protobuf object
        :param uuid_cache: uuid cache
        :returns: pygtirb object
        :rtype: AuxDataContainer

        """
        return cls(
            (key, AuxData._from_protobuf(val))
            for key, val in aux_data_container.aux_data.items()
        )


class Block:
    """
    A basic block.
    """

    def __init__(self, uuid=None,
                 address=0, size=0, decode_mode=0, uuid_cache=None):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
        if uuid_cache is not None:
            uuid_cache[uuid] = self
        self.address = address
        self.size = size
        self.decode_mode = decode_mode

    def _to_protobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = Block_pb2.Block()
        ret.uuid = self.uuid.bytes
        ret.address = self.address
        ret.size = self.size
        ret.decode_mode = self.decode_mode
        return ret

    @classmethod
    def _from_protobuf(cls, block, uuid_cache=None):
        """
        Load pygtirb class from protobuf class
        """
        uuid = UUID(bytes=block.uuid)
        if uuid_cache is not None and uuid in uuid_cache:
            return uuid_cache[uuid]
        return cls(uuid, block.address,
                   block.size, block.decode_mode, uuid_cache)


class ByteMap:
    """
    Holds the bytes of the loaded image of the binary.
    """

    def __init__(self, regions=None):
        if regions is None:
            regions = []
        self.regions = regions

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """

        ret = ByteMap_pb2.ByteMap()

        def region_to_protobuf(region):
            reg = ByteMap_pb2.Region()
            reg.address, reg.data = region
            return reg

        ret.regions.extend(region_to_protobuf(r) for r in self.regions)
        return ret

    @classmethod
    def _from_protobuf(cls, byte_map, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        return cls([(region.address, region.data)
                    for region in byte_map.regions])

    def addRegion(self, addr, data):
        """ Add region to this ByteMap """
        self.regions.append((addr, data))


class CFG:
    """
    Control Flow Graphs (CFGs)
    Interprocedural control flow graph, with vertices of type
    Block.
    """

    def __init__(self, edges=None, module=None):
        if edges is None:
            edges = set()
        self.edges = edges
        self.module = module

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = CFG_pb2.CFG()

        if self.module is not None:
            ret.vertices.extend(v.uuid.bytes for v in self.module.blocks)
            ret.vertices.extend(v.uuid.bytes for v in self.module.proxies)
        ret.edges.extend(e._to_protobuf() for e in self.edges)
        return ret

    @classmethod
    def _from_protobuf(cls, cfg, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        return cls({Edge._from_protobuf(e, uuid_cache) for e in cfg.edges})

    def add_vertex(self, vertex):
        """Add a Block/ProxyBlock vertex to CFG.

        :param vertex: the Block/ProxyBlock

        """
        if isinstance(vertex, Block):
            self.module.blocks.add(vertex)
        elif isinstance(vertex, ProxyBlock):
            self.module.proxies.add(vertex)

    def add_edge(self, edge):
        """ Add an Edge to the CFG """
        self.edges.add(edge)
        self.add_vertex(edge.source)
        self.add_vertex(edge.target)

    def remove_edges(self, edges):
        """ Remove a set of edges from the CFG """
        self.edges -= edges


class DataObject:
    """
    Represents a data object, possibly symbolic.

    Does not directly store the data bytes, which are kept in the
    ImageByteMap.
    """

    def __init__(self, uuid=None, address=0, size=0, uuid_cache=None):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
        if uuid_cache is not None:
            uuid_cache[uuid] = self
        self.address = address
        self.size = size

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = DataObject_pb2.DataObject()
        ret.uuid = self.uuid.bytes
        ret.address = self.address
        ret.size = self.size
        return ret

    @classmethod
    def _from_protobuf(cls, data_object, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=data_object.uuid)
        if uuid_cache is not None and uuid in uuid_cache:
            return uuid_cache[uuid]
        return cls(uuid, data_object.address, data_object.size, uuid_cache)


class Edge:
    """
    An Edge in the CFG. Consists of a source and target Block
    """

    def __init__(self, label, source, target):
        self.label = label
        self.source = source
        self.target = target

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = CFG_pb2.Edge()
        ret.source_uuid = self.source.uuid.bytes
        ret.target_uuid = self.target.uuid.bytes
        ret.label.CopyFrom(self.label._to_protobuf())
        return ret

    @classmethod
    def _from_protobuf(cls, edge, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        source_uuid = UUID(bytes=edge.source_uuid)
        target_uuid = UUID(bytes=edge.target_uuid)
        source = None
        target = None
        if uuid_cache is not None:
            source = uuid_cache.get(source_uuid)
            target = uuid_cache.get(target_uuid)
        return cls(EdgeLabel._from_protobuf(edge.label), source, target)


class EdgeLabel:
    """
    A label on a CFG edge.
    """
    class EdgeType(Enum):
        """
        Indicates the type of control flow transfer indicated by this
        edge.
        """
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

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = CFG_pb2.EdgeLabel()
        ret.conditional = self.conditional
        ret.direct = self.direct
        ret.type = self.type.value
        return ret

    @classmethod
    def _from_protobuf(cls, edge_label, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        return cls(edge_label.conditional, edge_label.direct,
                   EdgeLabel.EdgeType(edge_label.type))


class ImageByteMap:
    """
    Contains the loaded raw image data for the module (binary).
    """

    def __init__(self,
                 uuid=None,
                 byte_map=None,
                 addr_min=0,
                 addr_max=0,
                 base_address=0,
                 entry_point_address=0,
                 uuid_cache=None):
        if uuid is None:
            uuid = uuid4()
        if byte_map is None:
            byte_map = ByteMap()
        self.uuid = uuid
        if uuid_cache is not None:
            uuid_cache[uuid] = self
        self.byte_map = byte_map
        self.addr_min = addr_min
        self.addr_max = addr_max
        self.base_address = base_address
        self.entry_point_address = entry_point_address

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = ImageByteMap_pb2.ImageByteMap()
        ret.uuid = self.uuid.bytes
        ret.byte_map.CopyFrom(self.byte_map._to_protobuf())
        ret.addr_min = self.addr_min
        ret.addr_max = self.addr_max
        ret.base_address = self.base_address
        ret.entry_point_address = self.entry_point_address
        return ret

    @classmethod
    def _from_protobuf(cls, image_byte_map, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=image_byte_map.uuid)
        if uuid_cache is not None and uuid in uuid_cache:
            return uuid_cache[uuid]
        image_byte_map = cls(
            uuid=uuid,
            byte_map=ByteMap._from_protobuf(image_byte_map.byte_map),
            addr_min=image_byte_map.addr_min,
            addr_max=image_byte_map.addr_max,
            base_address=image_byte_map.base_address,
            entry_point_address=image_byte_map.entry_point_address,
            uuid_cache=uuid_cache)
        return image_byte_map


class IR(AuxDataContainer):
    """
    A complete internal representation consisting of multiple Modules.
    """

    def __init__(self, uuid=None, modules=None,
                 aux_data=None, uuid_cache=None):
        """IR constructor. Can be used to construct an empty IR instance

        :param uuid: UUID. Creates a new instance if None
        :param modules: List of modules
        :param aux_data: auxilary data hanging off the IR
        :param uuid_cache: uuid_cache
        :returns: IR
        :rtype: IR
        """
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
        if uuid_cache is not None:
            uuid_cache[uuid] = self
        if modules is None:
            modules = list()
        self.modules = modules
        super().__init__(aux_data)

    def _to_protobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = IR_pb2.IR()
        ret.uuid = self.uuid.bytes
        ret.modules.extend(m._to_protobuf() for m in self.modules)
        ret.aux_data_container.CopyFrom(super()._to_protobuf())
        return ret

    @classmethod
    def _from_protobuf(cls, protobuf_ir, uuid_cache=None):
        """Load pygtirb class from protobuf object

        :param cls: this class
        :param protobuf_ir: the protobuf IR object
        :returns: the pygtirb IR object
        :rtype: IR

        """
        uuid = UUID(bytes=protobuf_ir.uuid)
        uuid_cache = dict()
        modules = [Module._from_protobuf(m, uuid_cache)
                   for m in protobuf_ir.modules]
        aux_data = {
            key: AuxData._from_protobuf(val)
            for key, val in protobuf_ir.aux_data_container.aux_data.items()
        }
        ir = cls(uuid, modules, aux_data, uuid_cache)
        return ir

    @staticmethod
    def load_protobuf_file(protobuf_file):
        """Load IR from protobuf file object

        :param protobuf_file: The protobuf file object
        :returns: GTIR
        :rtype: IR

        """
        ir = IR_pb2.IR()
        ir.ParseFromString(protobuf_file.read())
        return IR._from_protobuf(ir)

    @staticmethod
    def load_protobuf(file_name):
        """Load IR from protobuf file at path.

        :param file_name: The given protobuf GTIR file path
        :returns: GTIR
        :rtype: IR
        """
        with open(file_name, 'rb') as f:
            return IR.load_protobuf_file(f)

    def save_protobuf_file(self, protobuf_file):
        """Save IR to protobuf file object

        :param protobuf_file: The protobuf file object
        """
        protobuf_file.write(self._to_protobuf().SerializeToString())

    def save_protobuf(self, file_name):
        """Save IR to protobuf file at path.

        :param file_name: The given protobuf GTIR file path
        """
        with open(file_name, 'wb') as f:
            self.save_protobuf_file(f)


class Module(AuxDataContainer):
    """
    The Module class represents loadable objects such as executables
    or libraries
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
                 aux_data=None,
                 binary_path='',
                 blocks=set(),
                 cfg=None,
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
                 uuid=None,
                 uuid_cache=None):
        """Constructor
        :param aux_data:
        :param binary_path:
        :param blocks:
        :param cfg:
        :param data:
        :param image_byte_map:
        :param isa_id:
        :param file_format:
        :param name:
        :param preferred_addr:
        :param proxies:
        :param rebase_delta:
        :param sections:
        :param symbols:
        :param symbolic_operands:
        :param uuid:
        :param uuid_cache:

        :returns: Module
        :rtype: Module

        """
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
        if uuid_cache is not None:
            uuid_cache[uuid] = self

        if image_byte_map is None:
            image_byte_map = ImageByteMap(uuid_cache=uuid_cache)

        self.binary_path = binary_path
        self.blocks = set(blocks)
        self.cfg = cfg
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

        super().__init__(aux_data)

    def _to_protobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """

        ret = Module_pb2.Module()
        ret.uuid = self.uuid.bytes
        ret.binary_path = self.binary_path
        ret.preferred_addr = self.preferred_addr
        ret.rebase_delta = self.rebase_delta
        ret.file_format = self.file_format
        ret.isa_id = self.isa_id
        ret.name = self.name
        ret.image_byte_map.CopyFrom(self.image_byte_map._to_protobuf())
        ret.symbols.extend(s._to_protobuf() for s in self.symbols)
        ret.cfg.CopyFrom(self.cfg._to_protobuf())
        ret.blocks.extend(b._to_protobuf() for b in self.blocks)
        ret.data.extend(d._to_protobuf() for d in self.data)
        ret.proxies.extend(p._to_protobuf() for p in self.proxies)
        ret.sections.extend(s._to_protobuf() for s in self.sections)

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
            ret.symbolic_operands[k].CopyFrom(sym_exp)

        ret.aux_data_container.CopyFrom(super()._to_protobuf())
        return ret

    @classmethod
    def _from_protobuf(cls, module, uuid_cache=None):
        """Load object from protobuf object

        :param cls: this class
        :param module: the protobuf module object
        :param uuid_cache: uuid cache
        :returns: newly instantiated pygtirb instance
        :rtype: Module

        """

        uuid = UUID(bytes=module.uuid)
        if uuid_cache is not None and uuid in uuid_cache:
            return uuid_cache[uuid]

        aux_data = (
            (k, AuxData._from_protobuf(v, uuid_cache))
            for k, v in module.aux_data_container.aux_data.items()
        )
        blocks = (Block._from_protobuf(b, uuid_cache) for b in module.blocks)
        cfg = CFG._from_protobuf(module.cfg, uuid_cache)
        proxies = \
            (ProxyBlock._from_protobuf(p, uuid_cache) for p in module.proxies)
        data = (DataObject._from_protobuf(d, uuid_cache) for d in module.data)
        image_byte_map = \
            ImageByteMap._from_protobuf(module.image_byte_map, uuid_cache)
        sections = \
            (Section._from_protobuf(s, uuid_cache) for s in module.sections)
        symbols = \
            (Symbol._from_protobuf(s, uuid_cache) for s in module.symbols)

        def sym_expr_from_protobuf(symbolic_expression):
            if symbolic_expression.HasField('stack_const'):
                return SymStackConst._from_protobuf(
                    symbolic_expression.stack_const, uuid_cache)
            if symbolic_expression.HasField('addr_const'):
                return SymAddrConst._from_protobuf(
                    symbolic_expression.addr_const, uuid_cache)
            if symbolic_expression.HasField('addr_addr'):
                return SymAddrAddr._from_protobuf(
                    symbolic_expression.addr_addr, uuid_cache)
        symbolic_operands = (
            (k, sym_expr_from_protobuf(v))
            for k, v in module.symbolic_operands.items()
        )

        module = cls(
            aux_data=aux_data,
            binary_path=module.binary_path,
            blocks=blocks,
            cfg=cfg,
            data=data,
            image_byte_map=image_byte_map,
            isa_id=module.isa_id,
            file_format=module.file_format,
            name=module.name,
            preferred_addr=module.preferred_addr,
            proxies=proxies,
            rebase_delta=module.rebase_delta,
            sections=sections,
            symbols=symbols,
            symbolic_operands=symbolic_operands,
            uuid=uuid,
            uuid_cache=uuid_cache
        )
        module.cfg.module = module
        return module

    def remove_blocks(self, blocks):
        """Remove blocks from the IR.

        :param blocks: a set of Blocks and ProxyBlocks to remove
        :returns: none
        :rtype: none

        """
        self.blocks -= blocks
        self.proxies -= blocks


class Offset:
    """
    Describes the location inside a block or data object.
    """

    def __init__(self, element_id, offset):
        self.element_id = element_id
        self.offset = offset

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = Offset_pb2.Offset()
        ret.element_id = self.element_id
        ret.offset = self.offset
        return ret

    @classmethod
    def _from_protobuf(cls, offset, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        return cls(offset.element_id, offset.offset)


class ProxyBlock:
    """
    A placeholder to serve as the endpoint of a CFG edge.

    A ProxyBlock exists in the CFG so that edges to or from another
    node may be constructed. For example, a call to a function in
    another module may be represented by an edge that originates at
    the calling block and targets a proxy. Another example would be an
    edge to represent an indirect jump whose target is not known.

    ProxyBlocks do not represent any instructions and so have neither
    an address nor a size.
    """

    def __init__(self, uuid=None, uuid_cache=None):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
        if uuid_cache is not None:
            uuid_cache[uuid] = self

    def _to_protobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = ProxyBlock_pb2.ProxyBlock()
        ret.uuid = self.uuid.bytes
        return ret

    @classmethod
    def _from_protobuf(cls, pb, uuid_cache=None):
        """Load pygtirb object from protobuf object

        :param cls: this class
        :param pb: protobuf proxyblock object
        :param uuid_cache: uuid cache
        :returns: pygtirb proxyblock object
        :rtype: ProxyBlock

        """
        uuid = UUID(bytes=pb.uuid)
        if uuid_cache is not None and uuid in uuid_cache:
            return uuid_cache[uuid]
        return cls(uuid, uuid_cache)


class Section:
    """
    Represents a named section of the binary.

    Does not directly store the contents of the section, which are
    kept in ImageByteMap.
    """

    def __init__(self, uuid=None, name='', address=0, size=0, uuid_cache=None):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
        if uuid_cache is not None:
            uuid_cache[uuid] = self
        self.name = name
        self.address = address
        self.size = size

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = Section_pb2.Section()
        ret.uuid = self.uuid.bytes
        ret.name = self.name
        ret.address = self.address
        ret.size = self.size
        return ret

    @classmethod
    def _from_protobuf(cls, section, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=section.uuid)
        if uuid_cache is not None and uuid in uuid_cache:
            return uuid_cache[uuid]
        return cls(uuid, section.name, section.address,
                   section.size, uuid_cache)


class Symbol:
    """
    Represents a Symbol, which maps a name to an object in the IR.
    """
    class StorageKind(Enum):
        """
        Indicates the storage kind of a Symbol.
        """
        Undefined = Symbol_pb2.StorageKind.Value('Storage_Undefined')
        Normal = Symbol_pb2.StorageKind.Value('Storage_Normal')
        Static = Symbol_pb2.StorageKind.Value('Storage_Static')
        Extern = Symbol_pb2.StorageKind.Value('Storage_Extern')
        Local = Symbol_pb2.StorageKind.Value('Storage_Local')

    def __init__(self,
                 uuid=None,
                 name='',
                 storage_kind=StorageKind.Undefined,
                 value=0,
                 referent=None,
                 uuid_cache=None):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
        if uuid_cache is not None:
            uuid_cache[uuid] = self
        self.value = value
        self.referent = referent
        self.name = name
        self.storage_kind = storage_kind

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = Symbol_pb2.Symbol()
        ret.uuid = self.uuid.bytes

        if self.value is not None:
            ret.value = self.value

        if self.referent is not None:
            ret.referent_uuid = self.referent.uuid.bytes

        ret.name = self.name
        ret.storage_kind = self.storage_kind.value
        return ret

    @classmethod
    def _from_protobuf(cls, symbol, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=symbol.uuid)
        if uuid_cache is not None and uuid in uuid_cache:
            return uuid_cache[uuid]
        value = None
        referent = None
        if symbol.HasField('value'):
            value = symbol.value
        if symbol.HasField('referent_uuid'):
            referent_uuid = UUID(bytes=symbol.referent_uuid)
            referent = None
            if uuid_cache is not None:
                referent = uuid_cache.get(referent_uuid)
        return cls(uuid, symbol.name, Symbol.StorageKind(symbol.storage_kind),
                   value, referent, uuid_cache)


class SymAddrAddr:
    """
    Represents a "symbolic operand" of the form
    "(Sym1 - Sym2) / Scale + Offset"
    """

    def __init__(self, scale, offset, symbol1, symbol2):
        self.scale = scale
        self.offset = offset
        self.symbol1 = symbol1
        self.symbol2 = symbol2

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = SymbolicExpression_pb2.SymAddrAddr()
        ret.scale = self.scale
        ret.offset = self.offset
        ret.symbol1_uuid = self.symbol1.uuid.bytes
        ret.symbol2_uuid = self.symbol2.uuid.bytes
        return ret

    @classmethod
    def _from_protobuf(cls, sym_addr_addr, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        symbol1_uuid = UUID(bytes=sym_addr_addr.symbol1_uuid)
        symbol2_uuid = UUID(bytes=sym_addr_addr.symbol2_uuid)
        symbol1 = None
        symbol2 = None
        if uuid_cache is not None:
            symbol1 = uuid_cache.get(symbol1_uuid)
            symbol2 = uuid_cache.get(symbol2_uuid)
        return cls(sym_addr_addr.scale, sym_addr_addr.offset, symbol1, symbol2)


class SymAddrConst:
    """
    Represents a "symbolic operand" of the form "Sym + Offset".
    """
    def __init__(self, offset, symbol):
        self.offset = offset
        self.symbol = symbol

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = SymbolicExpression_pb2.SymAddrConst()
        ret.offset = self.offset
        if self.symbol is not None:
            ret.symbol_uuid = self.symbol.uuid.bytes
        return ret

    @classmethod
    def _from_protobuf(cls, sym_addr_const, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        symbol_uuid = UUID(bytes=sym_addr_const.symbol_uuid)
        symbol = None
        if uuid_cache is not None:
            symbol = uuid_cache.get(symbol_uuid)
        return cls(sym_addr_const.offset, symbol)


class SymStackConst:
    """
    Represents a "symbolic operand" of the form "Sym + Offset",
    representing an offset from a stack variable.
    """
    def __init__(self, offset, symbol):
        self.offset = offset
        self.symbol = symbol

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = SymbolicExpression_pb2.SymStackConst()
        ret.offset = self.offset
        if self.symbol is not None:
            ret.symbol_uuid = self.symbol.uuid.bytes
        return ret

    @classmethod
    def _from_protobuf(cls, sym_stack_const, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        symbol_uuid = UUID(bytes=sym_stack_const.symbol_uuid)
        symbol = None
        if uuid_cache is not None:
            symbol = uuid_cache.get(symbol_uuid)
        return cls(sym_stack_const.offset, symbol)
