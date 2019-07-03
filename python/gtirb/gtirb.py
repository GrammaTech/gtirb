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
import json
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


# GTIRB CLASSES #


class AuxDataContainer:
    """
    Contains the AuxData Tables and serves as a base class
    """

    def __init__(self, aux_data=None):
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
    def _from_protobuf(cls, uuid_cache, aux_data_container):
        """Load pygtirb object from protobuf object

        :param cls: this class
        :param uuid_cache: uuid cache
        :param aux_data_container: protobuf object
        :returns: pygtirb object
        :rtype: AuxDataContainer

        """
        return cls({
            key: AuxData._from_protobuf(uuid_cache, val)
            for key, val in aux_data_container.aux_data.items()
        })


class Module(AuxDataContainer):
    """
    The Module class represents loadable objects such as executables
    or libraries
    """

    def __init__(self,
                 uuid_cache,
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
                 aux_data=None):
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
            sections = []
        if symbols is None:
            symbols = []
        if symbolic_operands is None:
            symbolic_operands = {}
        if aux_data is None:
            aux_data = {}
        if uuid is None:
            uuid = uuid4()

        # FIXME: We really want to use sets for `symbols` &
        # `sections` but we have fragile tests that depend on
        # preserving the order of these.

        self.uuid = uuid
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

        super().__init__(aux_data=aux_data)

    def _to_protobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """

        def _symbolicExpressionToProtobuf(v):
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
            ret.symbolic_operands[k].CopyFrom(_symbolicExpressionToProtobuf(v))

        ret.aux_data_container.CopyFrom(super()._to_protobuf())
        return ret

    @classmethod
    def _from_protobuf(cls, uuid_cache, module):
        """Load object from protobuf object

        :param cls: this class
        :param uuid_cache: uuid cache
        :param module: the protobuf module object
        :returns: newly instantiated pygtirb instance
        :rtype: Module

        """

        def symbolicExpressionFromProtobuf(uuid_cache, symbolic_expression):
            if symbolic_expression.HasField('stack_const'):
                return SymStackConst._from_protobuf(
                    uuid_cache, symbolic_expression.stack_const)
            if symbolic_expression.HasField('addr_const'):
                return SymAddrConst._from_protobuf(
                    uuid_cache, symbolic_expression.addr_const)
            if symbolic_expression.HasField('addr_addr'):
                return SymAddrAddr._from_protobuf(
                    uuid_cache, symbolic_expression.addr_addr)

        uuid = UUID(bytes=module.uuid)
        if uuid in uuid_cache:
            return uuid_cache[uuid]

        blocks = {Block._from_protobuf(uuid_cache, blk) for blk in module.blocks}
        proxy_blocks = \
            {ProxyBlock._from_protobuf(uuid_cache, pb) for pb in module.proxies}
        data = {DataObject._from_protobuf(uuid_cache, dt) for dt in module.data}
        symbols = [Symbol._from_protobuf(uuid_cache, sym) for sym in module.symbols]
        module = cls(
            uuid_cache,
            uuid=uuid,
            binary_path=module.binary_path,
            preferred_addr=module.preferred_addr,
            rebase_delta=module.rebase_delta,
            file_format=module.file_format,
            isa_id=module.isa_id,
            name=module.name,
            image_byte_map=ImageByteMap._from_protobuf(
                uuid_cache, module.image_byte_map),
            symbols=symbols,
            cfg=CFG._from_protobuf(uuid_cache, module.cfg),
            blocks=blocks,
            data=data,
            proxies=proxy_blocks,
            sections=[
                Section._from_protobuf(uuid_cache, sec)
                for sec in module.sections
            ],
            symbolic_operands={
                key: symbolicExpressionFromProtobuf(uuid_cache, se)
                for key, se in module.symbolic_operands.items()
            },
            aux_data={
                key: AuxData._from_protobuf(uuid_cache, val)
                for key, val in module.aux_data_container.aux_data.items()
            })
        module.cfg.module = module
        return module

    def remove_blocks(self, blocks):
        """Remove blocks from the IR.

        :param blocks_to_remove: a list of Blocks to remove
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

    def __init__(self, uuid_cache, uuid=None):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
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
    def _from_protobuf(cls, uuid_cache, pb):
        """Load pygtirb object from protobuf object

        :param cls: this class
        :param uuid_cache: uuid cache
        :param pb: protobuf proxyblock object
        :returns: pygtirb proxyblock object
        :rtype: ProxyBlock

        """
        uuid = UUID(bytes=pb.uuid)
        if uuid in uuid_cache:
            return uuid_cache[uuid]
        return cls(uuid_cache, uuid)


class AuxData:
    """
    Types and operations for auxiliar data.  AuxData objects can be
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
    def _from_protobuf(cls, uuid_cache, aux_data):
        """
        Load pygtirb class from protobuf class
        """
        ret = serializer.decode(aux_data.type_name, io.BytesIO(aux_data.data))
        return cls(aux_data.type_name, ret)


class Block:
    """
    A basic block.
    """

    def __init__(self, uuid_cache, uuid=None, address=0, size=0, decode_mode=0):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
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
    def _from_protobuf(cls, uuid_cache, block):
        """
        Load pygtirb class from protobuf class
        """
        uuid = UUID(bytes=block.uuid)
        if uuid in uuid_cache:
            return uuid_cache[uuid]
        return cls(uuid_cache, uuid, block.address, block.size, block.decode_mode)


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
    def _from_protobuf(cls, uuid_cache, byte_map):
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
    def _from_protobuf(cls, uuid_cache, edge_label):
        """
        Load this cls from protobuf object
        """
        return cls(edge_label.conditional, edge_label.direct,
                   EdgeType(edge_label.type))


class Edge:
    """
    An Edge in the CFG. Consists of a source and target Block
    """

    def __init__(self, label, source_block, target_block):
        self.source_block = source_block
        self.target_block = target_block
        self.label = label

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = CFG_pb2.Edge()
        ret.source_uuid = self.source_block.uuid.bytes
        ret.target_uuid = self.target_block.uuid.bytes
        ret.label.CopyFrom(self.label._to_protobuf())
        return ret

    @classmethod
    def _from_protobuf(cls, uuid_cache, edge):
        """
        Load this cls from protobuf object
        """
        source_uuid = UUID(bytes=edge.source_uuid)
        target_uuid = UUID(bytes=edge.target_uuid)
        return cls(EdgeLabel._from_protobuf(uuid_cache, edge.label),
                   uuid_cache.get(source_uuid),
                   uuid_cache.get(target_uuid))


class CFG:
    """
    Control Flow Graphs (CFGs)
    Interprocedural control flow graph, with vertices of type
    Block.
    """

    def __init__(self, edges, module=None):
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
    def _from_protobuf(cls, uuid_cache, cfg):
        """
        Load this cls from protobuf object
        """
        return cls({Edge._from_protobuf(uuid_cache, e) for e in cfg.edges})

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

    def __init__(self, uuid_cache, uuid=None, address=0, size=0):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
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
    def _from_protobuf(cls, uuid_cache, data_object):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=data_object.uuid)
        if uuid in uuid_cache:
            return uuid_cache[uuid]
        return cls(uuid_cache, uuid, data_object.address, data_object.size)


class ImageByteMap:
    """
    Contains the loaded raw image data for the module (binary).
    """

    def __init__(self,
                 uuid_cache,
                 uuid=None,
                 byte_map=None,
                 addr_min=0,
                 addr_max=0,
                 base_address=0,
                 entry_point_address=0):
        if uuid is None:
            uuid = uuid4()
        if byte_map is None:
            byte_map = ByteMap()
        self.uuid = uuid
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
    def _from_protobuf(cls, uuid_cache, image_byte_map):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=image_byte_map.uuid)
        if uuid in uuid_cache:
            return uuid_cache[uuid]
        image_byte_map = cls(
            uuid_cache,
            uuid=uuid,
            byte_map=ByteMap._from_protobuf(uuid_cache, image_byte_map.byte_map),
            addr_min=image_byte_map.addr_min,
            addr_max=image_byte_map.addr_max,
            base_address=image_byte_map.base_address,
            entry_point_address=image_byte_map.entry_point_address)
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
    def _from_protobuf(cls, uuid_cache, offset):
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

    def __init__(self, uuid_cache, uuid=None, name='', address=0, size=0):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
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
    def _from_protobuf(cls, uuid_cache, section):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=section.uuid)
        if uuid in uuid_cache:
            return uuid_cache[uuid]
        return cls(uuid_cache, uuid, section.name, section.address, section.size)


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
    def _from_protobuf(cls, uuid_cache, symbol):
        """
        Load this cls from protobuf object
        """
        if symbol.uuid != b'':
            symbol_uuid = UUID(bytes=symbol.uuid)
            return cls(symbol.offset, uuid_cache.get(symbol_uuid))
        else:
            return cls(symbol.offset)


class SymAddrConst:
    """
    Represents a "symbolic operand" of the form "Sym + Offset".
    """

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
    def _from_protobuf(cls, uuid_cache, symbol):
        """
        Load this cls from protobuf object
        """
        if symbol.uuid != b'':
            symbol_uuid = UUID(bytes=symbol.uuid)
            return cls(symbol.offset, uuid_cache.get(symbol_uuid))
        else:
            return cls(symbol.offset)


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
    def _from_protobuf(cls, uuid_cache, symbol):
        """
        Load this cls from protobuf object
        """
        return cls(
            symbol.scale, symbol.offset,
            uuid_cache.get(UUID(bytes=symbol.symbol1_uuid)),
            uuid_cache.get(UUID(bytes=symbol.symbol2_uuid))
        )


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
                 uuid_cache,
                 uuid=None,
                 name='',
                 storage_kind=StorageKind.Undefined,
                 value=0,
                 referent=None):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
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
    def _from_protobuf(cls, uuid_cache, symbol):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=symbol.uuid)
        if uuid in uuid_cache:
            return uuid_cache[uuid]
        value = None
        referent = None
        if symbol.HasField('value'):
            value = symbol.value
        if symbol.HasField('referent_uuid'):
            referent = uuid_cache.get(UUID(bytes=symbol.referent_uuid))
        return cls(uuid_cache, uuid, symbol.name,
                   Symbol.StorageKind(symbol.storage_kind), value, referent)


class IR(AuxDataContainer):
    """
    A complete internal representation consisting of multiple Modules.
    """

    def __init__(self, uuid_cache, uuid=None, modules=None, aux_data=None):
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
        uuid_cache[uuid] = self
        if modules is None:
            modules = set()
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
    def _from_protobuf(cls, uuid_cache, ir):
        """Load pygtirb class from protobuf object

        :param cls: this class
        :param uuid_cache: uuid cache
        :param ir: the protobuf IR object
        :returns: the pygtirb IR object
        :rtype: IR

        """
        uuid = UUID(bytes=ir.uuid)
        if uuid in uuid_cache:
            return uuid_cache[uuid]

        modules = [Module._from_protobuf(uuid_cache, m) for m in ir.modules]
        ir = cls(uuid_cache,
                 uuid,
                 modules,
                 aux_data={
                     key: AuxData._from_protobuf(uuid_cache, val)
                     for key, val in ir.aux_data_container.aux_data.items()
                 })
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
        return IR._from_protobuf(dict(), ir)

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

