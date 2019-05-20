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

import sys
import json
import uuid
from enum import Enum

import os
dir_path = os.path.dirname(os.path.realpath(__file__))
sys.path.append(dir_path)

import AuxDataContainer_pb2
import AuxData_pb2
import Block_pb2
import ByteMap_pb2
import CFG_pb2
import DataObject_pb2
import ImageByteMap_pb2
import InstructionRef_pb2
import IR_pb2
import Module_pb2
import ProxyBlock_pb2
import Section_pb2
import SymbolicExpression_pb2
import Symbol_pb2

# The global serializer instance. User can use this to register new
# encoders/decoders.
import serialization
serializer = serialization.Serialization()


# Do we even need this?
def _uuidToBytes(uuid):
    """uuid to bytes in little-endian.

    :param uuid: 
    :returns: bytes in little endian form
    :rtype: bytes

    """
    if uuid is None:
        return b''

    return uuid.bytes


def _uuidFromBytes(b):
    """
    Get UUID from bytes
    :param b: bytes
    :returns: a uuid.UUID
    :rtype: a uuid.UUID

    """
    if b == b'':
        return None
    else:
        return uuid.UUID(bytes=b)


class GTIRBTypeEncoder(json.JSONEncoder):
    """
    A class used to encode types so they can be serialized to json format.
    """

    def default(self, obj):
        if (isinstance(obj, AuxDataContainer) or isinstance(obj, Block)
                or isinstance(obj, ByteMap) or isinstance(obj, CFG)
                or isinstance(obj, DataObject) or isinstance(obj, EdgeLabel)
                or isinstance(obj, Edge) or isinstance(obj, Factory)
                or isinstance(obj, ImageByteMap)
                or isinstance(obj, InstructionRef) or isinstance(obj, IR)
                or isinstance(obj, Module) or isinstance(obj, ProxyBlock)
                or isinstance(obj, Region) or isinstance(obj, Section)
                or isinstance(obj, SymAddrAddr)
                or isinstance(obj, SymAddrConst) or isinstance(obj, Symbol)
                or isinstance(obj, SymbolicExpression)
                or isinstance(obj, SymStackConst)):
            return obj.__dict__
        elif isinstance(obj, AuxData):
            obj._data = self.default(obj._data)
            return obj.__dict__
        elif isinstance(obj, dict):
            final = {}
            for k, v in obj.items():
                final[self.default(k)] = self.default(v)
            return final
        elif isinstance(obj, uuid.UUID):
            # if the obj is uuid, we simply return the value of uuid
            return obj.hex

        return obj


class Factory(object):
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


########### GTIRB CLASSES ##############


class Addr(object):
    """
    A special class to store an Effective Address.
    
    It is a thin wrapper around a uint64_t for 64-bit address storage. Its
    semantics in overflow situations are the same as semantics for unsigned
    integers.
    
    An Addr cannot store a relative address as it cannot contain a negative
    number.
    """

    def __init__(self):
        self._address = None

    def __init__(self, address):
        self._address = address


class Module(object):
    '''
    The Module class represents loadable objects such as executables
    or libraries
    '''

    def __init__(self, uuid, binary_path, preferred_addr, rebase_delta,
                 file_format, isa_id, name, image_byte_map, symbols, cfg,
                 blocks, data, proxies, sections, symbolic_operands,
                 aux_data_container):
        """Constructor, takes the params below.

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
        self._uuid = uuid
        self._binary_path = binary_path
        self._preferred_addr = preferred_addr
        self._rebase_delta = rebase_delta
        self._file_format = file_format
        self._isa_id = isa_id
        self._name = name
        self._image_byte_map = image_byte_map
        self._symbols = symbols
        self._cfg = cfg
        self._blocks = blocks
        self._data = data
        self._proxies = proxies
        self._sections = sections
        self._symbolic_operands = symbolic_operands
        self._aux_data_container = aux_data_container

    @classmethod
    def create(cls, factory):
        """Create an empty Module

        :param cls: The Module class
        :param factory: The factory instance
        :returns: A newly created empty Module
        :rtype: Module

        """
        muuid = uuid.uuid4()
        ret = cls(muuid,
                  '',
                  binary_path='',
                  Addr(),
                  rebase_delta=0,
                  file_format='',
                  isa_id=None,
                  name='',
                  image_byte_map=None,
                  symbols=[],
                  cfg=None,
                  blocks=[],
                  data=[],
                  proxies=[],
                  sections=[],
                  symbolic_operands={},
                  aux_data_container=AuxDataContainer())

        factory.addObject(muuid, ret)
        return ret

    def getAuxData(self, name):
        """Get the AuxData by a particular name.

        :param name: AuxData key
        :returns: the underlying AuxData instance
        :rtype: AuxData

        """
        assert name in self._aux_data_container._aux_data,\
            "No AuxData found for key %s."%(name)
        return self._aux_data_container._aux_data[name]._data

    def toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = Module_pb2.Module()
        ret.uuid = _uuidToBytes(self._uuid)

        ret.binary_path = self._binary_path
        ret.preferred_addr = self._preferred_addr
        ret.rebase_delta = self._rebase_delta
        ret.file_format = self._file_format
        ret.isa_id = self._isa_id
        ret.name = self._name
        ret.image_byte_map.CopyFrom(self._image_byte_map.toProtobuf())
        ret.symbols.extend([s.toProtobuf() for s in self._symbols])
        ret.cfg.CopyFrom(self._cfg.toProtobuf())
        ret.blocks.extend([b.toProtobuf() for b in self._blocks])
        dd = [d.toProtobuf() for d in self._data]
        ret.data.extend(dd)
        ret.proxies.extend([p.toProtobuf() for p in self._proxies])
        ret.sections.extend([s.toProtobuf() for s in self._sections])
        for k, v in self._symbolic_operands.items():
            ret.symbolic_operands[k].CopyFrom(v.toProtobuf())

        ret.aux_data_container.CopyFrom(self._aux_data_container.toProtobuf())
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _module):
        """Load object from protobuf object

        :param cls: this class
        :param _factory: the factory to check for uuid uniqueness
        :param _module: the protobuf module object
        :returns: newly instantiated pygtirb instance
        :rtype: Module

        """
        uuid = _uuidFromBytes(_module.uuid)
        module = _factory.objectForUuid(uuid)
        if module is None:
            module = cls(
                uuid, _module.binary_path, _module.preferred_addr,
                _module.rebase_delta, _module.file_format, _module.isa_id,
                _module.name,
                ImageByteMap.fromProtobuf(_factory, _module.image_byte_map), [
                    Symbol.fromProtobuf(_factory, sym)
                    for sym in _module.symbols
                ], CFG.fromProtobuf(_factory, _module.cfg),
                [Block.fromProtobuf(_factory, blk) for blk in _module.blocks],
                [DataObject.fromProtobuf(_factory, dt)
                 for dt in _module.data], [
                     ProxyBlock.fromProtobuf(_factory, pb)
                     for pb in _module.proxies
                 ], [
                     Section.fromProtobuf(_factory, sec)
                     for sec in _module.sections
                 ], {
                     key: SymbolicExpression.fromProtobuf(_factory, se)
                     for key, se in _module.symbolic_operands.items()
                 },
                AuxDataContainer.fromProtobuf(_factory,
                                              _module.aux_data_container))

            _factory.addObject(uuid, module)

        return module

    def removeBlocks(self, blocks_to_remove):
        """Remove blocks from the IR.

        :param blocks_to_remove: a list of Blocks to remove
        :returns: none
        :rtype: none

        """
        for block_to_remove in blocks_to_remove:
            self._blocks.remove(block_to_remove)

    def addBlock(self, block):
        """Add a block to the. Needs to do a linear scan of current
        list of blocks.

        :param block: A Block
        :returns: none
        :rtype: none

        """
        if block not in self._blocks:
            self._blocks.append(block)


class IR(object):
    '''
    A complete internal representation consisting of multiple Modules.
    '''

    def __init__(self, uuid, modules=[],
                 aux_data_container=AuxDataContainer()):
        self._uuid = uuid
        self._modules = modules
        self._aux_data_container = aux_data_container

    @classmethod
    def create(cls, factory):
        """create an empty IR instance

        :param cls: IR
        :param factory: the factory instance
        :returns: a newly created IR instance
        :rtype: IR

        """
        iruuid = uuid.uuid4()
        ret = cls(iruuid)
        factory.addObject(iruuid, ret)
        return ret

    def toProtobuf(self):
        """Returns protobuf representation of the object
    
        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = IR_pb2.IR()
        ret.uuid = _uuidToBytes(self._uuid)
        ret.modules.extend([m.toProtobuf() for m in self._modules])
        ret.aux_data_container.CopyFrom(self._aux_data_container.toProtobuf())
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _ir):
        """Load pygtirb class from protobuf object

        :param cls: this class
        :param _factory: the factory to check for uuid uniqueness
        :param _ir: the protobuf IR object
        :returns: the pygtirb IR object
        :rtype: IR

        """
        uuid = _uuidFromBytes(_ir.uuid)
        ir = _factory.objectForUuid(uuid)
        if ir is not None:
            return ir

        modules = []
        for module in _ir.modules:
            modules.append(Module.fromProtobuf(_factory, module))

        aux_data_container = AuxDataContainer.fromProtobuf(
            _factory, _ir.aux_data_container)

        ir = cls(uuid, modules, aux_data_container)
        _factory.addObject(uuid, ir)
        return ir

    def __repr__(self):
        return str(self.__dict__)


class ProxyBlock(object):
    '''
    A placeholder to serve as the endpoint of a CFG edge.
    
    A ProxyBlock exists in the CFG so that edges to or from another
    node may be constructed. For example, a call to a function in
    another module may be represented by an edge that originates at
    the calling block and targets a proxy. Another example would be an
    edge to represent an indirect jump whose target is not known.

    ProxyBlocks do not represent any instructions and so have neither
    an address nor a size.
    '''

    def __init__(self, uuid):
        self._uuid = uuid

    @classmethod
    def create(cls, factory):
        """Create an empty ProxyBlock
        """
        puuid = uuid.uuid4()
        ret = cls(puuid)
        factory.addObject(puuid, ret)
        return ret

    def toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = ProxyBlock_pb2.ProxyBlock()
        ret.uuid = _uuidToBytes(self._uuid)
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _pb):
        """Load pygtirb object from protobuf object

        :param cls: this class
        :param _factory: uuid factory
        :param _pb: protobuf proxyblock object
        :returns: pygtirb proxyblock object
        :rtype: ProxyBlock

        """
        uuid = _uuidFromBytes(_pb.uuid)
        pb = _factory.objectForUuid(uuid)
        if pb is None:
            pb = cls(uuid)
            _factory.addObject(uuid, pb)

        return pb


class AuxDataContainer(object):
    '''
    Contains the AuxData Tables and serves as a base class
    '''

    def __init__(self, aux_data={}):
        self._aux_data = aux_data

    def toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = AuxDataContainer_pb2.AuxDataContainer()
        for k, v in self._aux_data.items():
            ret.aux_data[k].CopyFrom(v.toProtobuf())
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _aux_data_container):
        """Load pygtirb object from protobuf object

        :param cls: this class
        :param _factory: uuid factory
        :param _aux_data_container: protobuf object
        :returns: pygtirb object
        :rtype: AuxDataContainer

        """
        return cls({
            key: AuxData.fromProtobuf(_factory, val)
            for (key, val) in _aux_data_container.aux_data.items()
        })


class AuxData(object):
    '''
    Types and operations for auxiliar data.  AuxData objects can be
    attached to the IR or individual Modules to store additional
    client-specific data in a portable way.
    '''

    def __init__(self, type_name='', data=None):
        self._type_name = type_name
        self._data = data

    def toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = AuxData_pb2.AuxData()

        _bytes = b''
        _out_bytes_array = []
        _check_type_name = serializer.encode(_out_bytes_array, self._data)

        if _check_type_name != self._type_name:
            if self._type_name == None or self._type_name == '':
                # TODO: This is the case when there is empty data
                # structure somewhere in the aux data tree.
                raise NotImplementedError("Don't handle encoding empty"
                                          "aux data containers on pygtirb yet")
            else:
                _check_type_name = self._type_name

        ret.type_name = _check_type_name

        for _out_bytes in _out_bytes_array:
            _bytes += _out_bytes

        ret.data = _bytes
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _aux_data):
        """
        Load pygtirb class from protobuf class
        """
        ret = serializer.decode(_aux_data.type_name, bytearray(_aux_data.data))
        return cls(_aux_data.type_name, ret[0])


class Block(object):
    '''
    A basic block.
    '''

    def __init__(self, uuid, address, size, decode_mode):
        self._uuid = uuid
        self._address = address
        self._size = size
        self._decode_mode = decode_mode

    @classmethod
    def create(cls, factory, address, size, decode_mode):
        '''
        Create a Block with the given parameters.
        '''
        buuid = uuid.uuid4()
        blk = cls(buuid, address, size, decode_mode)
        factory.addObject(buuid, blk)
        return blk

    def toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = Block_pb2.Block()
        ret.uuid = _uuidToBytes(self._uuid)
        ret.address = self._address
        ret.size = self._size
        ret.decode_mode = self._decode_mode
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _block):
        """
        Load pygtirb class from protobuf class
        """
        uuid = _uuidFromBytes(_block.uuid)
        block = _factory.objectForUuid(uuid)
        if block is None:
            block = cls(uuid, _block.address, _block.size, _block.decode_mode)
            _factory.addObject(uuid, block)

        return block

    def __key(self):
        return tuple(v for k, v in sorted(self.__dict__.items()))

    def __hash__(self):
        return hash(self.__key())

    def __eq__(self, other):
        return self.__dict__ == other.__dict__


class Region(object):
    '''
    An address Region
    '''

    def __init__(self, address, data):
        self._address = address
        self._data = data

    def toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = ByteMap_pb2.Region()
        ret.address = self._address
        ret.data = self._data
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _region):
        """
        Load this class from protobuf object
        """
        return cls(_region.address, _region.data)


class ByteMap(object):
    '''
    Holds the bytes of the loaded image of the binary. 
    '''

    def __init__(self, regions=[]):
        self._regions = regions

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = ByteMap_pb2.ByteMap()
        ret.regions.extend([r.toProtobuf() for r in self._regions])
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _byte_map):
        """
        Load this cls from protobuf object
        """
        return cls([
            Region.fromProtobuf(_factory, region)
            for region in _byte_map.regions
        ])


class EdgeType(Enum):
    '''
    Indicates the type of control flow transfer indicated by this
    edge.
    '''
    Type_Branch = CFG_pb2.EdgeType.Value('Type_Branch')
    Type_Call = CFG_pb2.EdgeType.Value('Type_Call')
    Type_Fallthrough = CFG_pb2.EdgeType.Value('Type_Fallthrough')
    Type_Return = CFG_pb2.EdgeType.Value('Type_Return')
    Type_Syscall = CFG_pb2.EdgeType.Value('Type_Syscall')
    Type_Sysret = CFG_pb2.EdgeType.Value('Type_Sysret')


class EdgeLabel(object):
    '''
    A label on a CFG edge.
    '''

    def __init__(self, conditional, direct, type):
        self._conditional = conditional
        self._direct = direct
        self._type = type

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = CFG_pb2.EdgeLabel()
        ret.conditional = self._conditional
        ret.direct = self._direct
        ret.type = self._type.value
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _edge_label):
        """
        Load this cls from protobuf object
        """
        return cls(_edge_label.conditional, _edge_label.direct,
                   EdgeType(_edge_label.type))


class Edge(object):
    '''
    An Edge in the CFG. Consists of a source and target Block
    '''

    def __init__(self,
                 source_uuid,
                 target_uuid,
                 label,
                 source_block=None,
                 target_block=None):
        self._source_block = source_block
        self._target_block = target_block

        self._source_uuid = source_uuid
        self._target_uuid = target_uuid
        self._label = label

    def source(self):
        return self._source_block

    def target(self):
        return self._target_block

    def setSource(self, src):
        self._source_block = src

    def setTarget(self, tgt):
        self._target_block = tgt

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = CFG_pb2.Edge()
        ret.source_uuid = _uuidToBytes(self._source_uuid)
        ret.target_uuid = _uuidToBytes(self._target_uuid)
        ret.label.CopyFrom(self._label.toProtobuf())
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _edge):
        """
        Load this cls from protobuf object
        """
        return cls(_uuidFromBytes(_edge.source_uuid),
                   _uuidFromBytes(_edge.target_uuid),
                   EdgeLabel.fromProtobuf(_factory, _edge.label))

    def __key(self):
        return tuple(v for k, v in sorted(self.__dict__.items()))

    def __hash__(self):
        return hash(self.__key())

    def __eq__(self, other):
        return self.__dict__ == other.__dict__


class CFG(object):
    '''
    Control Flow Graphs (CFGs)
    Interprocedural control flow graph, with vertices of type
    Block.
    '''

    def __init__(self, vertices, edges):
        self._vertices = vertices
        self._edges = edges

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = CFG_pb2.CFG()
        ret.vertices.extend(self._vertices)
        ret.edges.extend([e.toProtobuf() for e in self._edges])
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _cfg):
        """
        Load this cls from protobuf object
        """
        return cls(_cfg.vertices,
                   [Edge.fromProtobuf(_factory, e) for e in _cfg.edges])

    def removeEdges(self, edges_to_remove):
        for edge_to_remove in edges_to_remove:
            self._edges.remove(edge_to_remove)


class DataObject(object):
    '''
    Represents a data object, possibly symbolic.

    Does not directly store the data bytes, which are kept in the
    ImageByteMap.
    '''

    def __init__(self, uuid, address=None, size=None):
        self._uuid = uuid
        self._address = address
        self._size = size

    @classmethod
    def create(cls, factory, address=None, size=None):
        '''
        Create a new Data Object
        '''
        duuid = uuid.uuid4()
        do = cls(duuid, address, size)
        factory.addObject(duuid, do)
        return do

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = DataObject_pb2.DataObject()
        ret.uuid = _uuidToBytes(self._uuid)
        ret.address = self._address
        ret.size = self._size
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _data_object):
        """
        Load this cls from protobuf object
        """
        uuid = _uuidFromBytes(_data_object.uuid)
        data_object = _factory.objectForUuid(uuid)
        if data_object is None:
            data_object = cls(uuid, _data_object.address, _data_object.size)
            _factory.addObject(uuid, data_object)

        return data_object


class ImageByteMap(object):
    '''
    Contains the loaded raw image data for the module (binary).
    '''

    def __init__(self, uuid, byte_map, addr_min, addr_max, base_address,
                 entry_point_address):
        self._uuid = uuid
        self._byte_map = byte_map
        self._addr_min = addr_min
        self._addr_max = addr_max
        self._base_address = base_address
        self._entry_point_address = entry_point_address

    @classmethod
    def create(cls, factory):
        """Create an ImageByteMap in it's default state.
        """
        ibm_uuid = uuid.uuid4()
        ibm = cls(ibm_uuid, ByteMap(), Addr(), Addr(), Addr(), Addr())
        factory.addObject(ibm_uuid, ibm)
        return ibm

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = ImageByteMap_pb2.ImageByteMap()
        ret.uuid = _uuidToBytes(self._uuid)
        ret.byte_map.CopyFrom(self._byte_map.toProtobuf())
        ret.addr_min = self._addr_min._address
        ret.addr_max = self._addr_max._address
        ret.base_address = self._base_address._address
        ret.entry_point_address = self._entry_point_address._address
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _image_byte_map):
        """
        Load this cls from protobuf object
        """
        uuid = _uuidFromBytes(_image_byte_map.uuid)
        image_byte_map = _factory.objectForUuid(uuid)
        if image_byte_map is None:
            image_byte_map = cls(
                uuid, ByteMap.fromProtobuf(_factory, _image_byte_map.byte_map),
                Addr(_image_byte_map.addr_min), Addr(_image_byte_map.addr_max),
                Addr(_image_byte_map.base_address),
                Addr(_image_byte_map.entry_point_address))

            _factory.addObject(uuid, image_byte_map)

        return image_byte_map


class InstructionRef(object):
    '''
    Describes the location of an instruction.
    '''

    def __init__(self, block_id, offset):
        self._block_id = block_id
        self._offset = offset

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = InstructionRef_pb2.InstructionRef()
        ret.block_id = self._block_id
        ret.offset = self._offset
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _instruction_ref):
        """
        Load this cls from protobuf object
        """
        return cls(_instruction_ref.block_id, _instruction_ref.offset)


class Section(object):
    '''
    Represents a named section of the binary.

    Does not directly store the contents of the section, which are
    kept in ImageByteMap.
    '''

    def __init__(self, uuid, name, address, size):
        self._uuid = uuid
        self._name = name
        self._address = address
        self._size = size

    @classmethod
    def create(cls, factory, name='', address=Addr(0), size=0):
        ''' Create a Section with the given params
        '''
        suuid = uuid.uuid4()
        sec = cls(suuid, name, address, size)
        factory.addObject(suuid, sec)
        return sec

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = Section_pb2.Section()
        ret.uuid = _uuidToBytes(self._uuid)
        ret.name = self._name
        ret.address = self._address
        ret.size = self._size
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _section):
        """
        Load this cls from protobuf object
        """
        uuid = _uuidFromBytes(_section.uuid)
        section = _factory.objectForUuid(uuid)
        if section is None:
            section = cls(uuid, _section.name, _section.address, _section.size)
            _factory.addObject(uuid, section)

        return section


class SymStackConst(object):
    '''
    Represents a "symbolic operand" of the form "Sym + Offset",
    representing an offset from a stack variable.
    '''

    def __init__(self, offset, symbol_uuid=None, symbol=None):
        self._offset = offset
        self._symbol_uuid = symbol_uuid
        self._symbol = symbol

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = SymbolicExpression_pb2.SymStackConst()
        ret.offset = self._offset
        ret.symbol_uuid = _uuidToBytes(self._symbol_uuid)
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _sym_stack_const):
        """
        Load this cls from protobuf object
        """
        if _sym_stack_const.symbol_uuid != b'':
            return cls(_sym_stack_const.offset,
                       _uuidFromBytes(_sym_stack_const.symbol_uuid))
        else:
            return cls(_sym_stack_const.offset)


class SymAddrConst(object):
    '''
    Represents a "symbolic operand" of the form "Sym + Offset".
    '''

    def __init__(self, offset, symbol_uuid, symbol=None):
        self._offset = offset
        self._symbol_uuid = symbol_uuid
        self._symbol = symbol

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = SymbolicExpression_pb2.SymAddrConst()
        ret.offset = self._offset
        ret.symbol_uuid = _uuidToBytes(self._symbol_uuid)
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _sym_addr_const):
        """
        Load this cls from protobuf object
        """
        return cls(_sym_addr_const.offset,
                   _uuidFromBytes(_sym_addr_const.symbol_uuid))


class SymAddrAddr(object):
    '''
    Represents a "symbolic operand" of the form 
    "(Sym1 - Sym2) / Scale + Offset"
    '''

    def __init__(self, scale, offset, symbol1_uuid, symbol2_uuid):
        self._scale = scale
        self._offset = offset
        self._symbol1_uuid = symbol1_uuid
        self._symbol2_uuid = symbol2_uuid

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = SymbolicExpression_pb2.SymAddrAddr()
        ret.scale = self._scale
        ret.offset = self._offset
        ret.symbol1_uuid = _uuidToBytes(self._symbol1_uuid)
        ret.symbol2_uuid = _uuidToBytes(self._symbol2_uuid)
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _sym_addr_addr):
        """
        Load this cls from protobuf object
        """
        return cls(_sym_addr_addr.scale, _sym_addr_addr.offset,
                   _uuidFromBytes(_sym_addr_addr.symbol1_uuid),
                   _uuidFromBytes(_sym_addr_addr.symbol2_uuid))


class SymbolicExpression(object):
    '''
    A "symbolic expression".
    '''

    def __init__(self, stack_const=None, addr_const=None, addr_addr=None):
        self._stack_const = stack_const
        self._addr_const = addr_const
        self._addr_addr = addr_addr

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = SymbolicExpression_pb2.SymbolicExpression()
        if self._stack_const is not None:
            ret.stack_const.CopyFrom(self._stack_const.toProtobuf())

        if self._addr_const is not None:
            ret.addr_const.CopyFrom(self._addr_const.toProtobuf())

        if self._addr_addr is not None:
            ret.addr_addr.CopyFrom(self._addr_addr.toProtobuf())

        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _symbolic_expression):
        """
        Load this cls from protobuf object
        """
        stack_const = None
        addr_const = None
        addr_addr = None

        if _symbolic_expression.HasField('stack_const'):
            stack_const = getattr(_symbolic_expression, 'stack_const', None)
        if _symbolic_expression.HasField('addr_const'):
            addr_const = getattr(_symbolic_expression, 'addr_const', None)

        if _symbolic_expression.HasField('addr_addr'):
            addr_addr = getattr(_symbolic_expression, 'addr_addr', None)

        return cls(stack_const=(SymStackConst.fromProtobuf(
            _factory, stack_const) if stack_const is not None else None),
                   addr_const=(SymAddrConst.fromProtobuf(_factory, addr_const)
                               if addr_const is not None else None),
                   addr_addr=(SymAddrAddr.fromProtobuf(_factory, addr_addr)
                              if addr_addr is not None else None))


class StorageKind(Enum):
    '''
    Indicates the storage kind of a Symbol.
    '''
    Storage_Undefined = Symbol_pb2.StorageKind.Value('Storage_Undefined')
    Storage_Normal = Symbol_pb2.StorageKind.Value('Storage_Normal')
    Storage_Static = Symbol_pb2.StorageKind.Value('Storage_Static')
    Storage_Extern = Symbol_pb2.StorageKind.Value('Storage_Extern')
    Storage_Local = Symbol_pb2.StorageKind.Value('Storage_Local')


class Symbol(object):
    '''
    Represents a Symbol, which maps a name to an object in the IR.
    '''

    def __init__(self,
                 uuid,
                 name,
                 storage_kind,
                 value=None,
                 referent_uuid=None,
                 referent=None):
        self._uuid = uuid
        self._value = value
        self._referent_uuid = referent_uuid
        self._referent = referent
        self._name = name
        self._storage_kind = storage_kind

    @classmethod
    def create(cls,
               _factory,
               name,
               storage_kind,
               value=None,
               referent_uuid=None):
        suuid = uuid.uuid4()
        sym = cls(suuid, name, storage_kind, value, referent_uuid)
        _factory.addObject(suuid, sym)
        return sym

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = Symbol_pb2.Symbol()
        ret.uuid = _uuidToBytes(self._uuid)

        if self._value is not None:
            ret.value = self._value

        if self._referent_uuid is not None:
            ret.referent_uuid = _uuidToBytes(self._referent_uuid)

        ret.name = self._name
        ret.storage_kind = self._storage_kind.value
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _symbol):
        """
        Load this cls from protobuf object
        """
        uuid = _uuidFromBytes(_symbol.uuid)
        symbol = _factory.objectForUuid(uuid)
        if symbol is None:
            value = None
            referent_uuid = None

            if _symbol.HasField('value'):
                value = getattr(_symbol, 'value')

            if _symbol.HasField('referent_uuid'):
                referent_uuid = _uuidFromBytes(getattr(_symbol,
                                                      'referent_uuid'))

            symbol = cls(uuid, _symbol.name, StorageKind(_symbol.storage_kind),
                         value, referent_uuid)
            _factory.addObject(uuid, symbol)

        return symbol


def IRPrintString(protobuf_file):
    with open(protobuf_file, 'rb') as f:
        _ir = IR_pb2.IR()
        _ir.ParseFromString(f.read())
        print(_ir)


def IRLoadFromProtobuf(protobuf_file):
    with open(protobuf_file, 'rb') as f:
        _ir = IR_pb2.IR()
        _ir.ParseFromString(f.read())

        factory = Factory()
        ir = IR.fromProtobuf(factory, _ir)

        for module in ir._modules:
            cfg = module._cfg
            for edge in cfg._edges:
                _source = factory.objectForUuid(edge._source_uuid)
                _target = factory.objectForUuid(edge._target_uuid)
                assert _source is not None and _target is not None
                edge.setSource(_source)
                edge.setTarget(_target)

        return (ir, factory)


def main():
    file = sys.argv[1]
    (ir, factory) = IRLoadFromProtobuf(file)

    ir_out = ir.toProtobuf()
    f = open('out.gtir', "wb")
    f.write(ir_out.SerializeToString())
    f.close()
    #with open('out.json', 'w') as outfile:
    #   json.dump(ir, outfile, indent=2, cls=GTIRBEncoder)


if __name__ == "__main__":
    main()
