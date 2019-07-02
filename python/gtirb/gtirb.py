# -*- coding: utf-8 -*-
"""
The GTIRB python module.

Provides a python API into GTIRB, allowing you to open GTIR protobuf
files, manipulating them and writing them back.

Sample usage.

Opening a GTIR file and loading it into an IR instance

    file = '/path/to/GTIR_FILE'
    (ir, factory) = IRLoadFromProtobuf(file)

Writing back the ir instance as a protobuf file

    ir_out = ir.toProtobuf()
    f = open('out.gtir', "wb")
    f.write(ir_out.SerializeToString())
    f.close()

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


class Factory:
    """
    A class that stores a mapping from uuid -> object
    """

    def __init__(self):
        self._objects = {}

    def objectForUuid(self, uuid):
        """get object for uuid if present

        :param uuid: for uuid
        :returns: object if present, None otherwise
        :rtype: object

        """
        return self._objects.get(uuid, None)

    def addObject(self, uuid, obj):
        """add object for uuid. asserts that uuid is not present

        :param uuid: uuid of object to add
        :param obj: object to add

        """
        assert uuid not in self._objects
        self._objects[uuid] = obj


# GTIRB CLASSES #


class Addr:
    """
    A special class to store an Effective Address.

    It is an error to serialize any address that cannot fit in a 64bit
    unsigned int.
    """

    def __init__(self, address=None):
        self.address = address


class AuxDataContainer:
    """
    Contains the AuxData Tables and serves as a base class
    """

    def __init__(self, aux_data=None):
        if aux_data is None:
            self.aux_data = dict()
        else:
            self.aux_data = aux_data

    def toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = AuxDataContainer_pb2.AuxDataContainer()
        for k, v in self.aux_data.items():
            ret.aux_data[k].CopyFrom(v.toProtobuf())
        return ret

    @classmethod
    def fromProtobuf(cls, factory, aux_data_container):
        """Load pygtirb object from protobuf object

        :param cls: this class
        :param factory: uuid factory
        :param aux_data_container: protobuf object
        :returns: pygtirb object
        :rtype: AuxDataContainer

        """
        return cls({
            key: AuxData.fromProtobuf(factory, val)
            for key, val in aux_data_container.aux_data.items()
        })


class Module(AuxDataContainer):
    """
    The Module class represents loadable objects such as executables
    or libraries
    """

    def __init__(self,
                 factory,
                 *,
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
            image_byte_map = ImageByteMap(factory)
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

        factory.addObject(uuid, self)
        self.uuid = uuid
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

    def toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """

        def _symbolicExpressionToProtobuf(v):
            sym_exp = SymbolicExpression_pb2.SymbolicExpression()
            if isinstance(v, SymStackConst):
                sym_exp.stack_const.CopyFrom(v.toProtobuf())
            elif isinstance(v, SymAddrConst):
                sym_exp.addr_const.CopyFrom(v.toProtobuf())
            elif isinstance(v, SymAddrAddr):
                sym_exp.addr_addr.CopyFrom(v.toProtobuf())
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
        ret.image_byte_map.CopyFrom(self.image_byte_map.toProtobuf())
        ret.symbols.extend([s.toProtobuf() for s in self.symbols])
        ret.cfg.CopyFrom(self.cfg.toProtobuf())
        ret.blocks.extend([b.toProtobuf() for b in self.blocks])
        ret.data.extend([d.toProtobuf() for d in self.data])
        ret.proxies.extend([p.toProtobuf() for p in self.proxies])
        ret.sections.extend([s.toProtobuf() for s in self.sections])
        for k, v in self.symbolic_operands.items():
            ret.symbolic_operands[k].CopyFrom(_symbolicExpressionToProtobuf(v))

        ret.aux_data_container.CopyFrom(super().toProtobuf())
        return ret

    @classmethod
    def fromProtobuf(cls, factory, module):
        """Load object from protobuf object

        :param cls: this class
        :param factory: the factory to check for uuid uniqueness
        :param module: the protobuf module object
        :returns: newly instantiated pygtirb instance
        :rtype: Module

        """

        def symbolicExpressionFromProtobuf(factory, symbolic_expression):
            if symbolic_expression.HasField('stack_const'):
                return SymStackConst.fromProtobuf(
                    factory, symbolic_expression.stack_const)
            if symbolic_expression.HasField('addr_const'):
                return SymAddrConst.fromProtobuf(
                    factory, symbolic_expression.addr_const)
            if symbolic_expression.HasField('addr_addr'):
                return SymAddrAddr.fromProtobuf(
                    factory, symbolic_expression.addr_addr)

        uuid = UUID(bytes=module.uuid)
        if factory.objectForUuid(uuid) is not None:
            return factory.objectForUuid(uuid)

        blocks = {Block.fromProtobuf(factory, blk) for blk in module.blocks}
        proxy_blocks = \
            {ProxyBlock.fromProtobuf(factory, pb) for pb in module.proxies}
        data = {DataObject.fromProtobuf(factory, dt) for dt in module.data}
        symbols = [Symbol.fromProtobuf(factory, sym) for sym in module.symbols]
        module = cls(
            factory,
            uuid=uuid,
            binary_path=module.binary_path,
            preferred_addr=module.preferred_addr,
            rebase_delta=module.rebase_delta,
            file_format=module.file_format,
            isa_id=module.isa_id,
            name=module.name,
            image_byte_map=ImageByteMap.fromProtobuf(
                factory, module.image_byte_map),
            symbols=symbols,
            cfg=CFG.fromProtobuf(factory, module.cfg),
            blocks=blocks,
            data=data,
            proxies=proxy_blocks,
            sections=[
                Section.fromProtobuf(factory, sec)
                for sec in module.sections
            ],
            symbolic_operands={
                key: symbolicExpressionFromProtobuf(factory, se)
                for key, se in module.symbolic_operands.items()
            },
            aux_data={
                key: AuxData.fromProtobuf(factory, val)
                for key, val in module.aux_data_container.aux_data.items()
            })
        module.cfg.setModule(module)
        return module

    def removeBlocks(self, blocks_to_remove):
        """Remove blocks from the IR.

        :param blocks_to_remove: a list of Blocks to remove
        :returns: none
        :rtype: none

        """
        self.blocks -= blocks_to_remove
        self.proxies -= blocks_to_remove


class IR(AuxDataContainer):
    """
    A complete internal representation consisting of multiple Modules.
    """

    def __init__(self, factory, uuid=None, modules=None, aux_data=None):
        """IR constructor. Can be used to construct an empty IR instance

        :param uuid: UUID. Creates a new instance if None
        :param modules: List of modules
        :param aux_data: auxilary data hanging off the IR
        :param factory: The factory instance
        :returns: IR
        :rtype: IR
        """
        if uuid is None:
            uuid = uuid4()
        factory.addObject(uuid, self)
        self.uuid = uuid
        if modules is None:
            modules = set()
        self.modules = modules
        super().__init__(aux_data)

    def toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = IR_pb2.IR()
        ret.uuid = self.uuid.bytes
        ret.modules.extend([m.toProtobuf() for m in self.modules])
        ret.aux_data_container.CopyFrom(super().toProtobuf())
        return ret

    @classmethod
    def fromProtobuf(cls, factory, ir):
        """Load pygtirb class from protobuf object

        :param cls: this class
        :param factory: the factory to check for uuid uniqueness
        :param ir: the protobuf IR object
        :returns: the pygtirb IR object
        :rtype: IR

        """
        uuid = UUID(bytes=ir.uuid)
        if factory.objectForUuid(uuid) is not None:
            return factory.objectForUuid(uuid)

        modules = [Module.fromProtobuf(factory, m) for m in ir.modules]
        ir = cls(factory,
                 uuid,
                 modules,
                 aux_data={
                     key: AuxData.fromProtobuf(factory, val)
                     for key, val in ir.aux_data_container.aux_data.items()
                 })
        return ir


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

    def __init__(self, factory, uuid=None):
        if uuid is None:
            uuid = uuid4()
        factory.addObject(uuid, self)
        self.uuid = uuid

    def toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = ProxyBlock_pb2.ProxyBlock()
        ret.uuid = self.uuid.bytes
        return ret

    @classmethod
    def fromProtobuf(cls, factory, pb):
        """Load pygtirb object from protobuf object

        :param cls: this class
        :param factory: uuid factory
        :param pb: protobuf proxyblock object
        :returns: pygtirb proxyblock object
        :rtype: ProxyBlock

        """
        uuid = UUID(bytes=pb.uuid)
        if factory.objectForUuid(uuid) is not None:
            return factory.objectForUuid(uuid)
        return cls(factory, uuid)


class AuxData:
    """
    Types and operations for auxiliar data.  AuxData objects can be
    attached to the IR or individual Modules to store additional
    client-specific data in a portable way.
    """

    def __init__(self, type_name='', data=None):
        self.type_name = type_name
        self.data = data

    def toProtobuf(self):
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
    def fromProtobuf(cls, factory, aux_data):
        """
        Load pygtirb class from protobuf class
        """
        ret = serializer.decode(aux_data.type_name, io.BytesIO(aux_data.data))
        return cls(aux_data.type_name, ret)


class Block:
    """
    A basic block.
    """

    def __init__(self, factory, uuid=None, address=0, size=0, decode_mode=0):
        if uuid is None:
            uuid = uuid4()
        factory.addObject(uuid, self)
        self.uuid = uuid
        self.address = address
        self.size = size
        self.decode_mode = decode_mode

    def toProtobuf(self):
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
    def fromProtobuf(cls, factory, block):
        """
        Load pygtirb class from protobuf class
        """
        uuid = UUID(bytes=block.uuid)
        if factory.objectForUuid(uuid) is not None:
            return factory.objectForUuid(uuid)
        return cls(factory, uuid, block.address, block.size, block.decode_mode)


class ByteMap:
    """
    Holds the bytes of the loaded image of the binary.
    """

    def __init__(self, regions=None):
        if regions is None:
            regions = []
        self.regions = regions

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """

        ret = ByteMap_pb2.ByteMap()

        def region_to_protobuf(r_tuple):
            reg = ByteMap_pb2.Region()
            reg.address = r_tuple[0]
            reg.data = r_tuple[1]
            return reg

        ret.regions.extend([region_to_protobuf(r) for r in self.regions])
        return ret

    @classmethod
    def fromProtobuf(cls, factory, byte_map):
        """
        Load this cls from protobuf object
        """
        return cls([(region.address, region.data)
                    for region in byte_map.regions])

    def addRegion(self, addr, data):
        """ Add region to this ByteMap """
        self.regions.append((addr, data))


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


class EdgeLabel:
    """
    A label on a CFG edge.
    """

    def __init__(self, conditional, direct, type):
        self.conditional = conditional
        self.direct = direct
        self.type = type

    def toProtobuf(self):
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
    def fromProtobuf(cls, factory, edge_label):
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

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = CFG_pb2.Edge()
        ret.source_uuid = self.source_block.uuid.bytes
        ret.target_uuid = self.target_block.uuid.bytes
        ret.label.CopyFrom(self.label.toProtobuf())
        return ret

    @classmethod
    def fromProtobuf(cls, factory, edge):
        """
        Load this cls from protobuf object
        """
        return cls(EdgeLabel.fromProtobuf(factory, edge.label),
                   factory.objectForUuid(UUID(bytes=edge.source_uuid)),
                   factory.objectForUuid(UUID(bytes=edge.target_uuid)))


class CFG:
    """
    Control Flow Graphs (CFGs)
    Interprocedural control flow graph, with vertices of type
    Block.
    """

    def __init__(self, edges, module=None):
        self.edges = set(edges)
        self.module = module

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = CFG_pb2.CFG()

        blocks = [v.uuid.bytes for v in self.module.blocks]
        blocks.extend([v.uuid.bytes for v in self.module.proxies])
        ret.vertices.extend(blocks)
        ret.edges.extend([e.toProtobuf() for e in self.edges])
        return ret

    @classmethod
    def fromProtobuf(cls, factory, cfg):
        """
        Load this cls from protobuf object
        """
        return cls({Edge.fromProtobuf(factory, e) for e in cfg.edges})

    def addVertex(self, vertex):
        """Add a Block/ProxyBlock vertex to CFG.

        :param vertex: the Block/ProxyBlock

        """
        if isinstance(vertex, Block):
            self.module.blocks.add(vertex)
        elif isinstance(vertex, ProxyBlock):
            self.module.proxies.add(vertex)

    def addEdge(self, edge):
        """ Add an Edge to the CFG """
        if edge not in self.edges:
            self.edges.add(edge)

        self.addVertex(edge.source)
        self.addVertex(edge.target)

    def removeEdges(self, edges_to_remove):
        """ Remove a set of edges from the CFG """
        for edge_to_remove in edges_to_remove:
            self.edges.discard(edge_to_remove)


class DataObject:
    """
    Represents a data object, possibly symbolic.

    Does not directly store the data bytes, which are kept in the
    ImageByteMap.
    """

    def __init__(self, factory, uuid=None, address=0, size=0):
        if uuid is None:
            uuid = uuid4()
        factory.addObject(uuid, self)
        self.uuid = uuid
        self.address = address
        self.size = size

    def toProtobuf(self):
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
    def fromProtobuf(cls, factory, data_object):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=data_object.uuid)
        if factory.objectForUuid(uuid) is not None:
            return factory.objectForUuid(uuid)
        return cls(factory, uuid, data_object.address, data_object.size)


class ImageByteMap:
    """
    Contains the loaded raw image data for the module (binary).
    """

    def __init__(self,
                 factory,
                 *,
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
        factory.addObject(uuid, self)
        self.uuid = uuid
        self.byte_map = byte_map
        self.addr_min = addr_min
        self.addr_max = addr_max
        self.base_address = base_address
        self.entry_point_address = entry_point_address

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = ImageByteMap_pb2.ImageByteMap()
        ret.uuid = self.uuid.bytes
        ret.byte_map.CopyFrom(self.byte_map.toProtobuf())
        ret.addr_min = self.addr_min
        ret.addr_max = self.addr_max
        ret.base_address = self.base_address
        ret.entry_point_address = self.entry_point_address
        return ret

    @classmethod
    def fromProtobuf(cls, factory, image_byte_map):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=image_byte_map.uuid)
        if factory.objectForUuid(uuid) is not None:
            return factory.objectForUuid(uuid)
        image_byte_map = cls(
            factory,
            uuid=uuid,
            byte_map=ByteMap.fromProtobuf(factory, image_byte_map.byte_map),
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

    def toProtobuf(self):
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
    def fromProtobuf(cls, factory, offset):
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

    def __init__(self, factory, uuid=None, name='', address=0, size=0):
        if uuid is None:
            uuid = uuid4()
        factory.addObject(uuid, self)
        self.uuid = uuid
        self.name = name
        self.address = address
        self.size = size

    def toProtobuf(self):
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
    def fromProtobuf(cls, factory, section):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=section.uuid)
        if factory.objectForUuid(uuid) is not None:
            return factory.objectForUuid(uuid)
        return cls(factory, uuid, section.name, section.address, section.size)


class SymStackConst:
    """
    Represents a "symbolic operand" of the form "Sym + Offset",
    representing an offset from a stack variable.
    """
    def __init__(self, offset, symbol=None):
        self.offset = offset
        self.symbol = symbol

    def toProtobuf(self):
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
    def fromProtobuf(cls, factory, symbol):
        """
        Load this cls from protobuf object
        """
        if symbol.uuid != b'':
            return cls(symbol.offset,
                       factory.objectForUuid(UUID(bytes=symbol.uuid)))
        else:
            return cls(symbol.offset)


class SymAddrConst:
    """
    Represents a "symbolic operand" of the form "Sym + Offset".
    """

    def toProtobuf(self):
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
    def fromProtobuf(cls, factory, symbol):
        """
        Load this cls from protobuf object
        """
        if symbol.uuid != b'':
            return cls(symbol.offset,
                       factory.objectForUuid(UUID(bytes=symbol.uuid)))
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

    def toProtobuf(self):
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
    def fromProtobuf(cls, factory, symbol):
        """
        Load this cls from protobuf object
        """
        return cls(
            symbol.scale, symbol.offset,
            factory.objectForUuid(UUID(bytes=symbol.symbol1_uuid)),
            factory.objectForUuid(UUID(bytes=symbol.symbol2_uuid))
        )


class StorageKind(Enum):
    """
    Indicates the storage kind of a Symbol.
    """
    Undefined = Symbol_pb2.StorageKind.Value('Storage_Undefined')
    Normal = Symbol_pb2.StorageKind.Value('Storage_Normal')
    Static = Symbol_pb2.StorageKind.Value('Storage_Static')
    Extern = Symbol_pb2.StorageKind.Value('Storage_Extern')
    Local = Symbol_pb2.StorageKind.Value('Storage_Local')


class Symbol:
    """
    Represents a Symbol, which maps a name to an object in the IR.
    """

    def __init__(self,
                 factory,
                 uuid=None,
                 name='',
                 storage_kind=StorageKind.Undefined,
                 value=0,
                 referent=None):
        if uuid is None:
            uuid = uuid4()
        factory.addObject(uuid, self)
        self.uuid = uuid
        self.value = value
        self.referent = referent
        self.name = name
        self.storage_kind = storage_kind

    def toProtobuf(self):
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
    def fromProtobuf(cls, factory, symbol):
        """
        Load this cls from protobuf object
        """
        uuid = UUID(bytes=symbol.uuid)
        if factory.objectForUuid(uuid) is not None:
            return factory.objectForUuid(uuid)
        if symbol.HasField('value'):
            value = symbol.value
        else:
            value = None
        if symbol.HasField('referent_uuid'):
            referent = factory.objectForUuid(UUID(bytes=symbol.referent_uuid))
        else:
            referent = None
        return cls(factory, uuid, symbol.name,
                   StorageKind(symbol.storage_kind), value, referent)


def IRPrintString(protobuf_file):
    with open(protobuf_file, 'rb') as f:
        ir = IR_pb2.IR()
        ir.ParseFromString(f.read())
        print(ir)


class IRLoader:
    """
    Class used to load GTIR from protobuf format.
    """

    def __init__(self):
        self.ir = None
        self.factory = None

    def IRLoadFromProtobufFileName(self, protobuf_file):
        """Load IR from protobuf file at path.

        :param protobuf_file: The given protobuf GTIR file path
        :returns: GTIR
        :rtype: IR

        """
        assert self.ir is None, "IR already loaded in this IRLoader"
        with open(protobuf_file, 'rb') as f:
            return self.IRLoadFromProtobufFile(f)

    def IRLoadFromProtobufFile(self, f):
        """Load IR from protobuf file object

        :param protobuf_file: The given protobuf GTIR file path
        :returns: GTIR
        :rtype: IR

        """
        assert self.ir is None, "IR already loaded in this IRLoader"
        ir = IR_pb2.IR()
        ir.ParseFromString(f.read())
        self.factory = Factory()
        self.ir = IR.fromProtobuf(self.factory, ir)
        return self.ir

    def factory(self):
        assert self.factory is None, "Factory not loaded"
        return self.factory
