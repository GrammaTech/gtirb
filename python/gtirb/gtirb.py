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

    def __init__(self, type_name='', data=None):
        self.type_name = type_name
        self.data = data

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
    def _from_protobuf(cls, aux_data):
        """
        Load pygtirb class from protobuf class
        """
        ret = serializer.decode(aux_data.type_name, io.BytesIO(aux_data.data))
        return cls(aux_data.type_name, ret)


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
            self.aux_data = aux_data

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
        return cls({
            key: AuxData._from_protobuf(val)
            for key, val in aux_data_container.aux_data.items()
        })


class Module(AuxDataContainer):
    """
    The Module class represents loadable objects such as executables
    or libraries
    """

    def __init__(self,
                 uuid=None,
                 binary_path='',
                 preferred_addr=0,
                 rebase_delta=0,
                 file_format=Module_pb2.FileFormat.Value('Format_Undefined'),
                 isa_id=Module_pb2.ISAID.Value('ISA_Undefined'),
                 name='',
                 image_byte_map=None,
                 symbols=None,
                 cfg=None,
                 blocks=None,
                 data=None,
                 proxies=None,
                 sections=None,
                 symbolic_operands=None,
                 aux_data=None,
                 uuid_cache=None):
        """Constructor, takes the params below.
           Creates an empty module
        :param uuid:
        :param binary_path:
        :param preferred_addr:
        :param rebase_delta:
        :param file_format:
        :param isa_id:
        :param name:
        :param image_byte_map:
        :param symbols:
        :param cfg:
        :param blocks:
        :param data:
        :param proxies:
        :param sections:
        :param symbolic_operands:
        :param aux_data_container:
        :returns:
        :rtype:

        """
        if image_byte_map is None:
            image_byte_map = ImageByteMap(uuid_cache)
        if blocks is None:
            blocks = set()
        if data is None:
            data = set()
        if proxies is None:
            proxies = set()
        if sections is None:
            sections = set()
        if symbols is None:
            symbols = set()
        if symbolic_operands is None:
            symbolic_operands = dict()
        if uuid is None:
            uuid = uuid4()

        self.uuid = uuid
        if uuid_cache is not None:
            uuid_cache[uuid] = self
        self.binary_path = binary_path
        self.preferred_addr = preferred_addr
        self.rebase_delta = rebase_delta
        self.file_format = file_format
        self.isa_id = isa_id
        self.name = name
        self.image_byte_map = image_byte_map
        self.symbols = symbols
        self.cfg = cfg
        self.blocks = blocks
        self.proxies = proxies

        self.data = data
        self.sections = sections
        self.symbolic_operands = symbolic_operands

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

        def _sym_expr_to_protobuf(v):
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
            return sym_exp

        for k, v in self.symbolic_operands.items():
            ret.symbolic_operands[k].CopyFrom(_sym_expr_to_protobuf(v))

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

        def _sym_expr_from_protobuf(symbolic_expression, uuid_cache=None):
            if symbolic_expression.HasField('stack_const'):
                return SymStackConst._from_protobuf(
                    symbolic_expression.stack_const, uuid_cache)
            if symbolic_expression.HasField('addr_const'):
                return SymAddrConst._from_protobuf(
                    symbolic_expression.addr_const, uuid_cache)
            if symbolic_expression.HasField('addr_addr'):
                return SymAddrAddr._from_protobuf(
                    symbolic_expression.addr_addr, uuid_cache)

        uuid = UUID(bytes=module.uuid)
        if uuid_cache is not None and uuid in uuid_cache:
            return uuid_cache[uuid]

        blocks = {Block._from_protobuf(b, uuid_cache) for b in module.blocks}
        proxy_blocks = \
            {ProxyBlock._from_protobuf(p, uuid_cache) for p in module.proxies}
        data = {DataObject._from_protobuf(d, uuid_cache) for d in module.data}
        symbols = \
            {Symbol._from_protobuf(s, uuid_cache) for s in module.symbols}
        module = cls(
            uuid=uuid,
            binary_path=module.binary_path,
            preferred_addr=module.preferred_addr,
            rebase_delta=module.rebase_delta,
            file_format=module.file_format,
            isa_id=module.isa_id,
            name=module.name,
            image_byte_map=ImageByteMap._from_protobuf(
                module.image_byte_map, uuid_cache),
            symbols=symbols,
            cfg=CFG._from_protobuf(module.cfg, uuid_cache),
            blocks=blocks,
            data=data,
            proxies=proxy_blocks,
            sections=[
                Section._from_protobuf(sec, uuid_cache)
                for sec in module.sections
            ],
            symbolic_operands={
                key: _sym_expr_from_protobuf(se, uuid_cache)
                for key, se in module.symbolic_operands.items()
            },
            aux_data={
                key: AuxData._from_protobuf(val)
                for key, val in module.aux_data_container.aux_data.items()
            },
            uuid_cache=uuid_cache
        )
        module.cfg.module = module
        return module

    def remove_blocks(self, blocks):
        """Remove blocks from the IR.

        :param blocks_to_remove: a set of Blocks and ProxyBlocks to remove
        :returns: none
        :rtype: none

        """
        self.blocks -= blocks
        self.proxies -= blocks


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
    def _from_protobuf(cls, byte_map):
        """
        Load this cls from protobuf object
        """
        return cls([(region.address, region.data)
                    for region in byte_map.regions])

    def addRegion(self, addr, data):
        """ Add region to this ByteMap """
        self.regions.append((addr, data))


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
    def _from_protobuf(cls, edge_label):
        """
        Load this cls from protobuf object
        """
        return cls(edge_label.conditional, edge_label.direct,
                   EdgeLabel.EdgeType(edge_label.type))


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
    def _from_protobuf(cls, offset):
        """
        Load this cls from protobuf object
        """
        return cls(offset.element_id, offset.offset)


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


class SymStackConst:
    """
    Represents a "symbolic operand" of the form "Sym + Offset",
    representing an offset from a stack variable.
    """
    def __init__(self, offset, symbol=None):
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
        if sym_stack_const.symbol_uuid != b'':
            symbol_uuid = UUID(bytes=sym_stack_const.symbol_uuid)
            symbol = None
            if uuid_cache is not None:
                symbol = uuid_cache.get(symbol_uuid)
            return cls(sym_stack_const.offset, symbol)
        else:
            return cls(sym_stack_const.offset)


class SymAddrConst:
    """
    Represents a "symbolic operand" of the form "Sym + Offset".
    """
    def __init__(self, offset, symbol=None):
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
        if sym_addr_const.symbol_uuid != b'':
            symbol_uuid = UUID(bytes=sym_addr_const.symbol_uuid)
            symbol = None
            if uuid_cache is not None:
                symbol = uuid_cache.get(symbol_uuid)
            return cls(sym_addr_const.offset, symbol)
        else:
            return cls(sym_addr_const.offset)


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
    def _from_protobuf(cls, protobuf_ir):
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
        uuid_cache[uuid] = ir
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
