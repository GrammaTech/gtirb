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

from collections import OrderedDict
import sys
import json
import uuid
import io
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
        if (isinstance(obj,
                       (AuxDataContainer, Block, ByteMap, CFG, DataObject,
                        EdgeLabel, Edge, Factory, ImageByteMap, InstructionRef,
                        IR, Module, ProxyBlock, Region, Section, SymAddrAddr,
                        SymAddrConst, Symbol, SymStackConst))):
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

        return super().default(obj)


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

    def __init__(self, address=None):
        self._address = address


class AuxDataContainer(object):
    '''
    Contains the AuxData Tables and serves as a base class
    '''

    def __init__(self, aux_data=None):
        if aux_data is None:
            self._aux_data = {}
        else:
            self._aux_data = aux_data

    def _toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = AuxDataContainer_pb2.AuxDataContainer()
        for k, v in self._aux_data.items():
            ret.aux_data[k].CopyFrom(v._toProtobuf())
        return ret

    @classmethod
    def _fromProtobuf(cls, _factory, _aux_data_container):
        """Load pygtirb object from protobuf object

        :param cls: this class
        :param _factory: uuid factory
        :param _aux_data_container: protobuf object
        :returns: pygtirb object
        :rtype: AuxDataContainer

        """
        return cls({
            key: AuxData._fromProtobuf(_factory, val)
            for (key, val) in _aux_data_container.aux_data.items()
        })

    def auxData(self, name):
        """Get the AuxData by a particular name.

        :param name: AuxData key
        :returns: the underlying AuxData instance
        :rtype: AuxData

        """
        try:
            return self._aux_data[name]._data
        except KeyError as ke:
            raise ke

    def addAuxData(self, name, data):
        """Add the AuxData for a particular key.

        :param name: AuxData key
        :param data: The underlying data structure

        """
        self._aux_data[name] = _data


class Module(AuxDataContainer):
    '''
    The Module class represents loadable objects such as executables
    or libraries
    '''

    def __init__(self,
                 module_uuid=None,
                 binary_path='',
                 preferred_addr=0,
                 rebase_delta=0,
                 file_format='',
                 isa_id=None,
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
                 factory=None):
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
        if symbols is None:
            symbols = []
        if blocks is None:
            blocks = []
        if data is None:
            data = []
        if proxies is None:
            proxies = []
        if sections is None:
            sections = []
        if symbolic_operands is None:
            symbolic_operands = {}
        if aux_data is None:
            aux_data = {}
        if module_uuid is None:
            module_uuid = uuid.uuid4()
            factory.addObject(uuid, self)

        self._uuid = module_uuid
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

        super(Module, self).__init__(aux_data=aux_data)

    def _toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """

        def _symbolicExpressionToProtobuf(v):
            sym_exp = SymbolicExpression_pb2.SymbolicExpression()
            if isinstance(v, SymStackConst):
                sym_exp.stack_const.CopyFrom(v._toProtobuf())
            elif isinstance(v, SymAddrConst):
                sym_exp.addr_const.CopyFrom(v._toProtobuf())
            elif isinstance(v, SymAddrAddr):
                sym_exp.addr_addr.CopyFrom(v._toProtobuf())
            else:
                assert True, \
                    'Must be one of SymStackConst, SymAddrAddr or SymAddrConst'
            return sym_exp

        ret = Module_pb2.Module()
        ret.uuid = _uuidToBytes(self._uuid)

        ret.binary_path = self._binary_path
        ret.preferred_addr = self._preferred_addr
        ret.rebase_delta = self._rebase_delta
        ret.file_format = self._file_format
        ret.isa_id = self._isa_id
        ret.name = self._name
        ret.image_byte_map.CopyFrom(self._image_byte_map._toProtobuf())
        ret.symbols.extend([s._toProtobuf() for s in self._symbols])
        ret.cfg.CopyFrom(self._cfg._toProtobuf())
        ret.blocks.extend([b._toProtobuf() for b in self._blocks])
        dd = [d._toProtobuf() for d in self._data]
        ret.data.extend(dd)
        ret.proxies.extend([p._toProtobuf() for p in self._proxies])
        ret.sections.extend([s._toProtobuf() for s in self._sections])
        for k, v in self._symbolic_operands.items():
            ret.symbolic_operands[k].CopyFrom(_symbolicExpressionToProtobuf(v))

        ret.aux_data_container.CopyFrom(super(Module, self)._toProtobuf())
        return ret

    @classmethod
    def _fromProtobuf(cls, _factory, _module):
        """Load object from protobuf object

        :param cls: this class
        :param _factory: the factory to check for uuid uniqueness
        :param _module: the protobuf module object
        :returns: newly instantiated pygtirb instance
        :rtype: Module

        """

        def _symbolicExpressionFromProtobuf(_factory, _symbolic_expression):
            if _symbolic_expression.HasField('stack_const'):
                return SymStackConst._fromProtobuf(
                    _factory, getattr(_symbolic_expression, 'stack_const',
                                      None))
            if _symbolic_expression.HasField('addr_const'):
                return SymAddrConst._fromProtobuf(
                    _factory, getattr(_symbolic_expression, 'addr_const',
                                      None))
            if _symbolic_expression.HasField('addr_addr'):
                return SymAddrAddr._fromProtobuf(
                    _factory, getattr(_symbolic_expression, 'addr_addr', None))

        uuid = _uuidFromBytes(_module.uuid)
        module = _factory.objectForUuid(uuid)
        blocks = [Block._fromProtobuf(_factory, blk) for blk in _module.blocks]
        proxy_blocks = [
            ProxyBlock._fromProtobuf(_factory, pb) for pb in _module.proxies
        ]
        data_objects = [
            DataObject._fromProtobuf(_factory, dt) for dt in _module.data
        ]
        symbols = [
            Symbol._fromProtobuf(_factory, sym) for sym in _module.symbols
        ]
        if module is None:
            module = cls(
                uuid,
                _module.binary_path,
                _module.preferred_addr,
                _module.rebase_delta,
                _module.file_format,
                _module.isa_id,
                _module.name,
                ImageByteMap._fromProtobuf(_factory, _module.image_byte_map),
                symbols,
                CFG._fromProtobuf(_factory, _module.cfg),
                blocks,
                data_objects,
                proxy_blocks, [
                    Section._fromProtobuf(_factory, sec)
                    for sec in _module.sections
                ], {
                    key: _symbolicExpressionFromProtobuf(_factory, se)
                    for key, se in _module.symbolic_operands.items()
                },
                aux_data={
                    key: AuxData._fromProtobuf(_factory, val)
                    for (key,
                         val) in _module.aux_data_container.aux_data.items()
                })
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

        # Add to the CFG
        self._cfg.addVertex(block)

    def uuid(self):
        """ Get UUID of this Module """
        return self._uuid

    def setBinaryPath(self, _binary_path):
        """ Set the binary path of the Module """
        assert self._binary_path == '', \
            "Binary path is already set."

        self._binary_path = _binary_path

    def binaryPath(self):
        """ Get the binary path of the Module """
        return self._binary_path

    def setPreferredAddress(self, _preferred_addr):
        """ Set the preferred_addr of the Module """
        self._preferred_addr = _preferred_addr

    def preferredAddress(self):
        """ Get the preferred_addr of the Module """
        return self._preferred_addr

    def setRebaseDelta(self, rebase_delta):
        """ Set rebase_delta for this Module """
        self._rebase_delta = rebase_delta

    def rebaseDelta(self):
        """ Get rebase_delta for this Module """
        return self._rebase_delta

    def setFileFormat(self, file_format):
        """ Set file_format for this Module """
        self._file_format = file_format

    def fileFormat(self):
        """ Get file_format for this Module """
        return self._file_format

    def setIsaId(self, isa_id):
        """ Set isa_id for this Module """
        self._isa_id = isa_id

    def isaId(self):
        """ Get isa_id for this Module """
        return self._isa_id

    def setName(self, name):
        """ Set name for this Module """
        self._name = name

    def name(self):
        """ Get name for this Module """
        return self._name

    def setImageByteMap(self, image_byte_map):
        """ Set image_byte_map for this Module """
        self._image_byte_map = image_byte_map

    def imageByteMap(self):
        """ Get image_byte_map for this Module """
        return self._image_byte_map

    def addSymbol(self, symbol):
        """ Add symbol to Module's symbols IFF not already present """
        if symbol not in self._symbols:
            self._symbols.append(symbol)

    def symbols(self):
        """ Get symbols for this Module """
        return self._symbols

    def setCfg(self, cfg):
        """ Set cfg for this Module """
        self._cfg = cfg

    def cfg(self):
        """ Get cfg for this Module """
        return self._cfg

    def blocks(self):
        """ Get blocks for this Module """
        return self._blocks

    def addData(self, data):
        """ add data blocks to this Module """
        if data not in self._data:
            self._data.append(data)

    def data(self):
        """ Get DataObjects for this Module """
        return self._data

    def addProxyBlock(self, pblock):
        """ Add proxy block to this Module """
        if pblock not in self._proxies:
            self._proxies.append(pblock)

    def proxies(self):
        """ Get proxies for this Module """
        return self._proxies

    def addSection(self, section):
        """ Add section to this Module """
        if section not in self._sections:
            self._sections.append(section)

    def sections(self):
        """ Get sections for this Module """
        return self._sections

    def addSymbolicOperand(self, addr, symbolic_operand):
        """ add symbolic_operand for this addr """
        if addr not in self._symbolic_operands:
            self._symbolic_operands[addr] = symbolic_operand

    def symbolicOperands(self):
        """ Get symbolic_operands for this Module """
        return self._symbolic_operands


class IR(AuxDataContainer):
    '''
    A complete internal representation consisting of multiple Modules.
    '''

    def __init__(self, ir_uuid=None, modules=None, aux_data=None,
                 factory=None):
        """IR constructor. Can be used to construct an empty IR instance

        :param ir_uuid: UUID. Creates a new instance if None
        :param modules: List of modules
        :param aux_data: auxilary data hanging off the IR
        :param factory: The factory instance
        :returns: IR
        :rtype: IR

        """
        if ir_uuid is None:
            ir_uuid = uuid.uuid4()
            factory.addObject(ir_uuid, self)

        if modules is None:
            modules = []

        self._uuid = ir_uuid
        self._modules = modules
        super(IR, self).__init__(aux_data=aux_data)

    def toProtobuf(self):
        """Returns protobuf representation of the object
    
        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = IR_pb2.IR()
        ret.uuid = _uuidToBytes(self._uuid)
        ret.modules.extend([m._toProtobuf() for m in self._modules])
        ret.aux_data_container.CopyFrom(super(IR, self)._toProtobuf())
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
            modules.append(Module._fromProtobuf(_factory, module))

        ir = cls(uuid,
                 modules,
                 aux_data={
                     key: AuxData._fromProtobuf(_factory, val)
                     for (key, val) in _ir.aux_data_container.aux_data.items()
                 })

        _factory.addObject(uuid, ir)
        return ir

    def addModule(self, _module):
        """ Add module to the IR """
        if _module not in self._modules:
            self._modules.append(_module)

    def uuid(self):
        """ Get uuid for this IR """
        return self._uuid

    def modules(self):
        """ Get _modules  for this """
        return self._modules


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

    def __init__(self, proxy_uuid=None, factory=None):
        """
        ProxyBlock constructor. Can be used to create an empty ProxyBlock.
        """
        if proxy_uuid is None:
            proxy_uuid = uuid.uuid4()
            factory.addObject(proxy_uuid, self)

        self._uuid = proxy_uuid

    def _toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = ProxyBlock_pb2.ProxyBlock()
        ret.uuid = _uuidToBytes(self._uuid)
        return ret

    @classmethod
    def _fromProtobuf(cls, _factory, _pb):
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

    def uuid(self):
        """ Get uuid for this ProxyBlock """
        return self._uuid


class AuxData(object):
    '''
    Types and operations for auxiliar data.  AuxData objects can be
    attached to the IR or individual Modules to store additional
    client-specific data in a portable way.
    '''

    def __init__(self, type_name='', data=None):
        self._type_name = type_name
        self._data = data

    def _toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = AuxData_pb2.AuxData()

        _out_bytes_array = io.BytesIO()
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
        _out_bytes_array.seek(0)
        ret.data = _out_bytes_array.read()
        return ret

    @classmethod
    def _fromProtobuf(cls, _factory, _aux_data):
        """
        Load pygtirb class from protobuf class
        """
        ret = serializer.decode(_aux_data.type_name,
                                io.BytesIO(_aux_data.data))
        return cls(_aux_data.type_name, ret)


class Block(object):
    '''
    A basic block.
    '''

    def __init__(self,
                 block_uuid=None,
                 address=0,
                 size=0,
                 decode_mode=None,
                 factory=None):
        '''
        Constructor. Can be used to create a Block with the given parameters.
        '''
        if block_uuid is None:
            block_uuid = uuid.uuid4()
            factory.addObject(block_uuid, self)

        self._uuid = block_uuid
        self._address = address
        self._size = size
        self._decode_mode = decode_mode

    def _toProtobuf(self):
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
    def _fromProtobuf(cls, _factory, _block):
        """
        Load pygtirb class from protobuf class
        """
        uuid = _uuidFromBytes(_block.uuid)
        block = _factory.objectForUuid(uuid)
        if block is None:
            block = cls(uuid, _block.address, _block.size, _block.decode_mode)
            _factory.addObject(uuid, block)

        return block

    def uuid(self):
        """ Get uuid for this Block """
        return self._uuid

    def setAddress(self, address):
        """ Set address for this Block """
        self._address = address

    def address(self):
        """ Get address for this Block """
        return self._address

    def setSize(self, size):
        """ Set size for this Block """
        self._size = size

    def size(self):
        """ Get size for this Block """
        return self._size

    def setDecodeMode(self, decode_mode):
        """ Set decode_mode for this Block """
        self._decode_mode = decode_mode

    def decodeMode(self):
        """ Get decode_mode  for this Block """
        return self._decode_mode

    def _eq_key(self):
        return (self._address, self._size, self._decode_mode)

    def _key(self):
        return (self._uuid)

    def __hash__(self):
        return hash(self._key())

    def __eq__(self, other):
        return self._eq_key() == other._eq_key()


class ByteMap(object):
    '''
    Holds the bytes of the loaded image of the binary. 
    '''

    def __init__(self, regions=[]):
        self._regions = regions

    def _toProtobuf(self):
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

        ret.regions.extend([region_to_protobuf(r) for r in self._regions])
        return ret

    @classmethod
    def _fromProtobuf(cls, _factory, _byte_map):
        """
        Load this cls from protobuf object
        """
        return cls([(region.address, region.data)
                    for region in _byte_map.regions])

    def addRegion(self, addr, data):
        """ Add region to this ByteMap """
        self._regions.append((addr, data))

    def regions(self):
        """ Get regions for this ByteMap """
        return self._regions


class EdgeType(Enum):
    '''
    Indicates the type of control flow transfer indicated by this
    edge.
    '''
    Branch = CFG_pb2.EdgeType.Value('Type_Branch')
    Call = CFG_pb2.EdgeType.Value('Type_Call')
    Fallthrough = CFG_pb2.EdgeType.Value('Type_Fallthrough')
    Return = CFG_pb2.EdgeType.Value('Type_Return')
    Syscall = CFG_pb2.EdgeType.Value('Type_Syscall')
    Sysret = CFG_pb2.EdgeType.Value('Type_Sysret')


class EdgeLabel(object):
    '''
    A label on a CFG edge.
    '''

    def __init__(self, conditional, direct, type):
        self._conditional = conditional
        self._direct = direct
        self._type = type

    def _toProtobuf(self):
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
    def _fromProtobuf(cls, _factory, _edge_label):
        """
        Load this cls from protobuf object
        """
        return cls(_edge_label.conditional, _edge_label.direct,
                   EdgeType(_edge_label.type))

    def setConditional(self, conditional):
        """ Set conditional-ness for this EdgeLabel """
        self._conditional = conditional

    def conditional(self):
        """ Get conditional-ness for this EdgeLabel """
        return self._conditional

    def setDirect(self, direct):
        """ Set direct-ness for this EdgeLabel """
        self._direct = direct

    def direct(self):
        """ Get direct-ness for this EdgeLabel """
        return self._direct

    def setType(self, type):
        """ Set type for this EdgeLabel """
        self._type = type

    def type(self):
        """ Get type for this EdgeLabel """
        return self._type


class Edge(object):
    '''
    An Edge in the CFG. Consists of a source and target Block
    '''

    def __init__(self, label, source_block, target_block):
        self._source_block = source_block
        self._target_block = target_block

        self._label = label

    def setSource(self, source_block):
        """ Set source_block for this Edge """
        self._source_block = source_block

    def source(self):
        """ Get source_block for this Edge """
        return self._source_block

    def setTarget(self, target_block):
        """ Set target_block for this Edge """
        self._target_block = target_block

    def target(self):
        """ Get target_block for this Edge """
        return self._target_block

    def setLabel(self, label):
        """ Set label for this Edge """
        self._label = label

    def label(self):
        """ Get label for this Edge """
        return self._label

    def _toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = CFG_pb2.Edge()
        ret.source_uuid = _uuidToBytes(self._source_block.uuid())
        ret.target_uuid = _uuidToBytes(self._target_block.uuid())
        ret.label.CopyFrom(self._label._toProtobuf())
        return ret

    @classmethod
    def _fromProtobuf(cls, _factory, _edge):
        """
        Load this cls from protobuf object
        """
        return cls(EdgeLabel._fromProtobuf(_factory, _edge.label),
                   _factory.objectForUuid(_uuidFromBytes(_edge.source_uuid)),
                   _factory.objectForUuid(_uuidFromBytes(_edge.target_uuid)))

    def __key(self):
        return (self._source_block, self._target_block, self._label)

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

    def __init__(self, edges):
        self._vertices = OrderedDict()
        self._edges = edges

    def _toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = CFG_pb2.CFG()

        ret.vertices.extend([_uuidToBytes(v.uuid()) for v in self._vertices])
        ret.edges.extend([e._toProtobuf() for e in self._edges])
        return ret

    @classmethod
    def _fromProtobuf(cls, _factory, _cfg):
        """
        Load this cls from protobuf object
        """
        cfg = cls(set([Edge._fromProtobuf(_factory, e) for e in _cfg.edges]))

        for vertex_uuid_bytes in _cfg.vertices:
            vertex = _factory.objectForUuid(_uuidFromBytes(vertex_uuid_bytes))
            assert vertex is not None
            cfg.addVertex(vertex)

        return cfg

    def addVertex(self, vertex):
        """Add a Block/ProxyBlock vertex to CFG.

        :param vertex: the Block/ProxyBlock

        """
        self._vertices[vertex] = None

    def addEdge(self, edge):
        """ Add an Edge to the CFG """
        if edge not in self._edges:
            self._edges.add(edge)

        self.addVertex(edge.source())
        self.addVertex(edge.target())

    def removeEdges(self, edges_to_remove):
        """ Remove a set of edges from the CFG """
        for edge_to_remove in edges_to_remove:
            self._edges.discard(edge_to_remove)


class DataObject(object):
    '''
    Represents a data object, possibly symbolic.

    Does not directly store the data bytes, which are kept in the
    ImageByteMap.
    '''

    def __init__(self,
                 data_object_uuid=None,
                 address=None,
                 size=None,
                 factory=None):
        """Constructor. Can be used to create a DataObject.
        """
        if data_object_uuid is None:
            data_object_uuid = uuid.uuid4()
            factory.addObject(data_object_uuid, self)

        self._uuid = data_object_uuid
        self._address = address
        self._size = size

    def __key(self):
        return (self._address, self._size)

    def __hash__(self):
        return hash(self.__key())

    def __eq__(self, other):
        return self.__dict__ == other.__dict__

    def _toProtobuf(self):
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
    def _fromProtobuf(cls, _factory, _data_object):
        """
        Load this cls from protobuf object
        """
        uuid = _uuidFromBytes(_data_object.uuid)
        data_object = _factory.objectForUuid(uuid)
        if data_object is None:
            data_object = cls(uuid, _data_object.address, _data_object.size)
            _factory.addObject(uuid, data_object)

        return data_object

    def uuid(self):
        """ Get uuid for this DataObject """
        return self._uuid

    def setAddress(self, address):
        """ Set address for this DataObject """
        self._address = address

    def address(self):
        """ Get address for this DataObject """
        return self._address

    def setSize(self, size):
        """ Set size for this DataObject """
        self._size = size

    def size(self):
        """ Get size for this DataObject """
        return self._size


class ImageByteMap(object):
    '''
    Contains the loaded raw image data for the module (binary).
    '''

    def __init__(self,
                 image_byte_map_uuid=None,
                 byte_map=None,
                 addr_min=0,
                 addr_max=0,
                 base_address=0,
                 entry_point_address=0,
                 factory=None):
        """Constructor. Can be used to create an ImageByteMap with the given params.
        """
        if image_byte_map_uuid is None:
            image_byte_map_uuid = uuid.uuid4()
            factory.addObject(image_byte_map_uuid, self)

        if byte_map is None:
            byte_map = ByteMap()

        self._uuid = image_byte_map_uuid
        self._byte_map = byte_map
        self._addr_min = addr_min
        self._addr_max = addr_max
        self._base_address = base_address
        self._entry_point_address = entry_point_address

    def _toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = ImageByteMap_pb2.ImageByteMap()
        ret.uuid = _uuidToBytes(self._uuid)
        ret.byte_map.CopyFrom(self._byte_map._toProtobuf())
        ret.addr_min = self._addr_min._address
        ret.addr_max = self._addr_max._address
        ret.base_address = self._base_address._address
        ret.entry_point_address = self._entry_point_address._address
        return ret

    @classmethod
    def _fromProtobuf(cls, _factory, _image_byte_map):
        """
        Load this cls from protobuf object
        """
        uuid = _uuidFromBytes(_image_byte_map.uuid)
        image_byte_map = _factory.objectForUuid(uuid)
        if image_byte_map is None:
            image_byte_map = cls(
                uuid, ByteMap._fromProtobuf(_factory,
                                            _image_byte_map.byte_map),
                Addr(_image_byte_map.addr_min), Addr(_image_byte_map.addr_max),
                Addr(_image_byte_map.base_address),
                Addr(_image_byte_map.entry_point_address))

            _factory.addObject(uuid, image_byte_map)

        return image_byte_map

    def uuid(self):
        """ Get uuid for this ImageByteMap """
        return self._uuid

    def setByteMap(self, byte_map):
        """ Set byte_map for this ImageByteMap """
        self._byte_map = byte_map

    def byteMap(self):
        """ Get byte_map for this ImageByteMap """
        return self._byte_map

    def setAddrMin(self, addr_min):
        """ Set addr_min for this ImageByteMap """
        self._addr_min = addr_min

    def addrMin(self):
        """ Get addr_min for this ImageByteMap """
        return self._addr_min

    def setAddrMax(self, addr_max):
        """ Set addr_max for this ImageByteMap """
        self._addr_max = addr_max

    def addrMax(self):
        """ Get addr_max for this ImageByteMap """
        return self._addr_max

    def setBaseAddress(self, base_address):
        """ Set base_address for this ImageByteMap """
        self._base_address = base_address

    def baseAddress(self):
        """ Get base_address for this ImageByteMap """
        return self._base_address

    def setEntryPointAddress(self, entry_point_address):
        """ Set entry_point_address for this ImageByteMap """
        self._entry_point_address = entry_point_address

    def entryPointAddress(self):
        """ Get entry_point_address for this ImageByteMap """
        return self._entry_point_address


class InstructionRef(object):
    '''
    Describes the location of an instruction.
    '''

    def __init__(self, block_id, offset):
        self._block_id = block_id
        self._offset = offset

    def _toProtobuf(self):
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
    def _fromProtobuf(cls, _factory, _instruction_ref):
        """
        Load this cls from protobuf object
        """
        return cls(_instruction_ref.block_id, _instruction_ref.offset)

    def setBlockId(self, block_id):
        """ Set block_id for this InstrunctionRef """
        self._block_id = block_id

    def blockId(self):
        """ Get block_id for this InstrunctionRef """
        return self._block_id

    def setOffset(self, offset):
        """ Set offset for this InstrunctionRef """
        self._offset = offset

    def offset(self):
        """ Get offset for this InstrunctionRef """
        return self._offset


class Section(object):
    '''
    Represents a named section of the binary.

    Does not directly store the contents of the section, which are
    kept in ImageByteMap.
    '''

    def __init__(self,
                 section_uuid=None,
                 name='',
                 address=0,
                 size=0,
                 factory=None):
        """Constructor. Can be used to create a Section with the given params.
        """
        if section_uuid is None:
            section_uuid = uuid.uuid4()
            factory.addObject(section_uuid, self)

        self._uuid = section_uuid
        self._name = name
        self._address = address
        self._size = size

    def _toProtobuf(self):
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
    def _fromProtobuf(cls, _factory, _section):
        """
        Load this cls from protobuf object
        """
        uuid = _uuidFromBytes(_section.uuid)
        section = _factory.objectForUuid(uuid)
        if section is None:
            section = cls(uuid, _section.name, _section.address, _section.size)
            _factory.addObject(uuid, section)

        return section

    def uuid(self):
        """ Get uuid for this Section """
        return self._uuid

    def setName(self, name):
        """ Set name for this Section """
        self._name = name

    def name(self):
        """ Get name for this Section """
        return self._name

    def setAddress(self, address):
        """ Set address for this Section """
        self._address = address

    def address(self):
        """ Get address for this Section """
        return self._address

    def setSize(self, size):
        """ Set size for this Section """
        self._size = size

    def size(self):
        """ Get size for this Section """
        return self._size


class SymStackConst(object):
    '''
    Represents a "symbolic operand" of the form "Sym + Offset",
    representing an offset from a stack variable.
    '''

    def __init__(self, offset, symbol=None):
        self._offset = offset
        self._symbol = symbol

    def _toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = SymbolicExpression_pb2.SymStackConst()
        ret.offset = self._offset
        if self._symbol is not None:
            ret.symbol_uuid = _uuidToBytes(self._symbol.uuid())

        return ret

    @classmethod
    def _fromProtobuf(cls, _factory, _sym_stack_const):
        """
        Load this cls from protobuf object
        """
        if _sym_stack_const.symbol_uuid != b'':
            return cls(
                _sym_stack_const.offset,
                _factory.objectForUuid(
                    _uuidFromBytes(_sym_stack_const.symbol_uuid)))
        else:
            return cls(_sym_stack_const.offset)

    def setSymbol(self, symbol):
        """ Set symbol for this SymStackConst """
        return self._symbol

    def symbol(self):
        """ Get symbol for this SymStackConst """
        return self._symbol


class SymAddrConst(object):
    '''
    Represents a "symbolic operand" of the form "Sym + Offset".
    '''

    def __init__(self, offset, symbol=None):
        self._offset = offset
        self._symbol = symbol

    def _toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = SymbolicExpression_pb2.SymAddrConst()
        ret.offset = self._offset
        if self._symbol is not None:
            ret.symbol_uuid = _uuidToBytes(self._symbol.uuid())
        return ret

    @classmethod
    def _fromProtobuf(cls, _factory, _sym_addr_const):
        """
        Load this cls from protobuf object
        """
        return cls(
            _sym_addr_const.offset,
            _factory.objectForUuid(_uuidFromBytes(
                _sym_addr_const.symbol_uuid)))

    def setOffset(self, offset):
        """ Set offset for this SymAddrConst"""
        self._offset = offset

    def offset(self):
        """ Get offset for this SymAddrConst"""
        return self._offset

    def setSymbol(self, symbol):
        """ Set symbol for this SymAddrConst"""
        self._symbol = symbol

    def symbol(self):
        """ Get symbol for this SymAddrConst"""
        return self._symbol


class SymAddrAddr(object):
    '''
    Represents a "symbolic operand" of the form 
    "(Sym1 - Sym2) / Scale + Offset"
    '''

    def __init__(self, scale, offset, symbol1, symbol2):
        self._scale = scale
        self._offset = offset
        self._symbol1 = symbol1
        self._symbol2 = symbol2

    def _toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = SymbolicExpression_pb2.SymAddrAddr()
        ret.scale = self._scale
        ret.offset = self._offset
        ret.symbol1_uuid = _uuidToBytes(self._symbol1.uuid())
        ret.symbol2_uuid = _uuidToBytes(self._symbol2.uuid())
        return ret

    @classmethod
    def _fromProtobuf(cls, _factory, _sym_addr_addr):
        """
        Load this cls from protobuf object
        """
        return cls(
            _sym_addr_addr.scale, _sym_addr_addr.offset,
            _factory.objectForUuid(_uuidFromBytes(
                _sym_addr_addr.symbol1_uuid)),
            _factory.objectForUuid(_uuidFromBytes(
                _sym_addr_addr.symbol2_uuid)))

    def setScale(self, scale):
        """ Set scale for this SymAddrAddr"""
        self._scale = scale

    def scale(self):
        """ Get scale for this SymAddrAddr"""
        return self._scale

    def setOffset(self, offset):
        """ Set offset for this SymAddrAddr"""
        self._offset = offset

    def offset(self):
        """ Get offset for this SymAddrAddr"""
        return self._offset

    def setSymbol1(self, symbol1):
        """ Set symbol1 for this SymAddrAddr"""
        self._symbol1 = symbol1

    def symbol1(self):
        """ Get symbol1 for this SymAddrAddr"""
        return self._symbol1

    def setSymbol2(self, symbol2):
        """ Set symbol2 for this SymAddrAddr"""
        self._symbol2 = symbol2

    def symbol2(self):
        """ Get symbol2 for this SymAddrAddr"""
        return self._symbol2


class StorageKind(Enum):
    '''
    Indicates the storage kind of a Symbol.
    '''
    Undefined = Symbol_pb2.StorageKind.Value('Storage_Undefined')
    Normal = Symbol_pb2.StorageKind.Value('Storage_Normal')
    Static = Symbol_pb2.StorageKind.Value('Storage_Static')
    Extern = Symbol_pb2.StorageKind.Value('Storage_Extern')
    Local = Symbol_pb2.StorageKind.Value('Storage_Local')


class Symbol(object):
    '''
    Represents a Symbol, which maps a name to an object in the IR.
    '''

    def __init__(self,
                 symbol_uuid=None,
                 name='',
                 storage_kind=None,
                 value=None,
                 referent=None,
                 factory=None):
        if symbol_uuid is None:
            symbol_uuid = uuid.uuid4()
            factory.addObject(symbol_uuid, self)

        self._uuid = symbol_uuid
        self._value = value
        self._referent = referent
        self._name = name
        self._storage_kind = storage_kind

    def _toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = Symbol_pb2.Symbol()
        ret.uuid = _uuidToBytes(self._uuid)

        if self._value is not None:
            ret.value = self._value

        if self._referent is not None:
            ret.referent_uuid = _uuidToBytes(self._referent.uuid())

        ret.name = self._name
        ret.storage_kind = self._storage_kind.value
        return ret

    @classmethod
    def _fromProtobuf(cls, _factory, _symbol):
        """
        Load this cls from protobuf object
        """
        uuid = _uuidFromBytes(_symbol.uuid)
        symbol = _factory.objectForUuid(uuid)
        if symbol is None:
            value = None
            referent = None

            if _symbol.HasField('value'):
                value = getattr(_symbol, 'value')

            if _symbol.HasField('referent_uuid'):
                referent = _factory.objectForUuid(
                    _uuidFromBytes(getattr(_symbol, 'referent_uuid')))

            symbol = cls(uuid, _symbol.name, StorageKind(_symbol.storage_kind),
                         value, referent)
            _factory.addObject(uuid, symbol)

        return symbol

    def uuid(self):
        """ Get uuid for this Symbol """
        return self._uuid

    def setName(self, name):
        """ Set name for this Symbol """
        self._name = name

    def name(self):
        """ Get name for this Symbol """
        return self._name

    def setStorageKind(self, storage_kind):
        """ Set storage_kind for this Symbol """
        self._storage_kind = storage_kind

    def storageKind(self):
        """ Get storage_kind for this Symbol """
        return self._storage_kind

    def setValue(self, value):
        """ Set value for this Symbol """
        self._value = value

    def value(self):
        """ Get value for this Symbol """
        return self._value

    def setReferent(self, referent):
        """ Set referent for this Symbol"""
        self._referent = referent

    def referent(self):
        """ Get referent for this Symbol"""
        return self._referent


def IRPrintString(protobuf_file):
    with open(protobuf_file, 'rb') as f:
        _ir = IR_pb2.IR()
        _ir.ParseFromString(f.read())
        print(_ir)


class IRLoader(object):
    '''
    Class used to load GTIR from protobuf format.
    '''

    def __init__(self):
        self._ir = None
        self._factory = None

    def IRLoadFromProtobuf(self, protobuf_file):
        """Load IR from protobuf file at path.

        :param protobuf_file: The given protobuf GTIR file path
        :returns: GTIR
        :rtype: IR

        """
        with open(protobuf_file, 'rb') as f:
            _ir = IR_pb2.IR()
            _ir.ParseFromString(f.read())

            self._factory = Factory()
            self._ir = IR.fromProtobuf(self._factory, _ir)

            return self._ir
