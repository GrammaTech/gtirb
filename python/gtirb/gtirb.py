import sys
import json
import uuid

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


class Serialization(object):
    """
    A class used to encode/decode aux data table entries. Use the top
    level top `encode` and `decode` functions.
    """

    def debug(self, msg):
        """ Print debug messages when debugging is enabled

        :param msg: 
        :returns: 
        :rtype: 

        """
        if self.__debug:
            print(msg)
            
    def get_subtypes(self, type_name):
        """ Given an encoded aux_data type_name, get the parent type
        and it's sub types. Ex: 
        mapping<FOO,BAR> would return (mapping, [FOO, BAR])
        mapping<FOO,set<BAR>> would return (mapping, [FOO, set<BAR>])
        mapping<FOO,mapping<BAR,BAZ>> 
                       would return (mapping, [FOO,mapping<BAR,BAZ>]) 

        :param type_name: encoded type name. Must contain '<'
        :returns: a tuple of the form (TYPE, [SUB-TYPES...])
        :rtype: tuple

        """
        head = None
        subtypes = []
        depth = 0
        index = 0
        last_index = -1
        while True:
            c = type_name[index]
            if c == '<':
                if depth == 0:
                    head = type_name[0:index]
                    last_index = index
                    depth += 1
                else:
                    depth += 1
            elif c == '>':
                if depth == 1:
                    assert last_index != -1
                    subtypes.append(type_name[last_index+1:index])
                    break
                else:
                    depth -= 1
            elif c == ',':
                if depth == 1:
                    assert last_index != -1
                    subtypes.append(type_name[last_index+1:index])
                    last_index = index
            
            index += 1
            
        return (head, subtypes)


    def __init__(self):
        """
        Initializes all encoders and decoders.
        """
        self.__debug = False
        
        self._decoders = {
            'mapping': self.mapping_decoder,
            'sequence': self.sequence_decoder,
            'set': self.set_decoder,
            'string': self.string_decoder,
            'InstructionRef': self.iref_decoder,
            'UUID': self.uuid_decoder,
            'uint64_t': self.uint64_decoder,
            'Addr': self.addr_decoder
        }

        self._encoders = {
            'dict': self.mapping_encoder,
            'list': self.sequence_encoder,
            'set': self.set_encoder,
            'str': self.string_encoder,
            'InstructionRef': self.iref_encoder,
            'UUID': self.uuid_encoder,
            'uint64_t': self.uint64_encoder,
            'int': self.uint64_encoder,
            'Addr': self.addr_encoder
        }
    def register_encoder(self, type_name, encoder):
        assert type_name not in self._encoders, \
            '%s already has an encoder' %(type_name)
        
        self._encoders[type_name] = encoder

    def register_decoder(self, type_name, decoder):
        assert type_name not in self._decoders, \
            '%s already has a decoder' %(type_name)
        
        self._decoders[type_name] = decoder
                
    def mapping_decoder(self, _bytes, _sub_types):
        """decode a mapping<..> entry

        :param _bytes: Raw bytes
        :param _sub_types: List of subtype names
        :returns: tuple of the form (decoded return type, bytes to shift)
        :rtype: tuple

        """
        assert len(_sub_types) == 2
        
        shift = 0
        _key_type = _sub_types[0]
        _value_type = _sub_types[1]
        ret = {}
        
        (size, off) = self.uint64_decoder(_bytes)
        shift += off

        self.debug("<mapping> Size: %d %d" %(size, shift))

        for _ in range(0, size):
            (k, o) = self.decode(_key_type, _bytes[shift:])
            shift += o
            (v, o) = self.decode(_value_type, _bytes[shift:])
            shift += o
            ret[k] = v
            
        return (ret, shift)


    def mapping_encoder(self, _out, _map):
        """encode a dict into bytes.

        :param _out: output list of byte arrays.
        :param _map: map to encode
        :returns: the encoded type_name string
        :rtype: string

        """
        self.uint64_encoder(_out, len(_map))
        k_string = ''
        v_string = ''

        for k,v in _map.items():
            if k_string == '':
                k_string = self.encode(_out, k)
            else:
                assert k_string == self.encode(_out, k),\
                    "keys with different types present in this map"

            if v_string == '':
                v_string = self.encode(_out, v)
            else:
                assert v_string == self.encode(_out, v),\
                    "values with different types present in this map"

        return 'mapping<%s,%s>' %(k_string, v_string) 
        
        
    def set_decoder(self, _bytes, _sub_types):
        """decode a set<..> entry

        :param _bytes: Raw bytes
        :param _sub_types: a list of sub-type names
        :returns: tuple of the form (decoded return type, bytes to shift)
        :rtype: tuple

        """
        assert len(_sub_types) == 1
        
        shift = 0
        _type = _sub_types[0]
        ret = set()
        
        (size, off) = self.uint64_decoder(_bytes)
        shift += off
        
        for index in range(0, size):
            (v, o) = self.decode(_type, _bytes[shift:])
            shift += o
            ret.add(v)
            
        return (ret, shift)

    def set_encoder(self, _out, _set):
        """encode a set() to bytes

        :param _out: output list of byte arrays
        :param _set: set to encode
        :returns: encoded type name string
        :rtype: string

        """
        self.uint64_encoder(_out, len(_set))
        type_string = ''
        for item in (_set):
            if type_string == '':
                type_string = self.encode(_out, item)
            else:
                assert type_string == self.encode(_out, item),\
                     "values with different types present in this set"
                

        return 'set<%s>' %(type_string) 
        

    def sequence_decoder(self, _bytes, _sub_types):
        """decode a sequence<..> entry
        
        :param _bytes: Raw bytes
        :param _sub_types: a list of sub-type names
        :returns: tuple of the form (decoded return type, bytes to shift)
        :rtype: tuple

        """
        assert len(_sub_types) == 1
        
        shift = 0
        _type = _sub_types[0]
        ret = []
        
        (size, off) = self.uint64_decoder(_bytes)
        shift += off
        
        for index in range(0, size):
            (v, o) = self.decode(_type, _bytes[shift:])
            shift += o
            ret.append(v)
            
        return (ret, shift)
    
    def sequence_encoder(self, _out, _list):
        """encode a list to bytes

        :param _out: list of byte arrays
        :param _list: list to encode
        :returns: encoded type name string
        :rtype: string

        """
        self.uint64_encoder(_out, len(_list))
        type_string = ''
        for item in (_list):
            if type_string == '':
                type_string = self.encode(_out, item)
            else:
                assert type_string == self.encode(_out, item),\
                    "values with different types present in this sequence"
                
        return 'sequence<%s>' %(type_string) 
        
    def string_decoder(self, _bytes):
        """decode a string

        :param _bytes: Raw bytes
        :returns: tuple of the form (decoded return type, bytes to shift)
        :rtype: tuple

        """
        ret_off = 0
        (size, off) = self.uint64_decoder(_bytes)
        self.debug('string size: %d'%(size))
        self.debug('string str: %s'%(str(bytes(_bytes[off:off+size]))))
        
        return (str(bytes(_bytes[off:off+size]), 'utf-8'), off+size)
    
    def string_encoder(self, _out, _val):
        """encode a string to bytes

        :param _out: output list of byte arrays
        :param _val: string to encode
        :returns: "string"
        :rtype: string

        """
        self.uint64_encoder(_out, len(_val))
        _out.append(_val.encode())
        return 'string'
    
    def iref_decoder(self, _bytes):
        """decode an InstructionRef entry

        :param _bytes: Raw bytes
        :returns: tuple of the form (decoded return type, bytes to shift)
        :rtype: tuple

        """        
        ret_off = 0
        (bid, off) = self.uuid_decoder(_bytes)
        ret_off += off
        (offset, off) = self.uint64_decoder(_bytes[ret_off:])
        
        return (InstructionRef(bid, offset), ret_off+off) 

    def iref_encoder(self, _out, _val):
        """
        encode InstructionRef to bytes
        """
        
        self.uuid_encoder(_out, _val._block_id)
        self.uuid_encoder(_out, _val._offset)
        return 'InstructionRef'
        
    def uuid_decoder(self, _bytes):
        """decode a UUID entry

        :param _bytes: Raw bytes
        :returns: tuple of the form (decoded return type, bytes to shift)
        :rtype: tuple

        """
        if len(_bytes) < 16:
            return None
        return (uuid.UUID(bytes=bytes(_bytes[0:16])), 16)
    
    def uuid_encoder(self, _out, _val):
        """encode UUID to bytes

        :param _out: output list of byte arrays
        :param _val: uuid to encode
        :returns: "UUID"
        :rtype: string

        """
        _out.append(_val.bytes)
        return 'UUID'
                    
    def addr_decoder(self, _bytes):
        """decode an InstructionRef entry

        :param _bytes: Raw bytes
        :returns: tuple of the form (decoded return type, bytes to shift)
        :rtype: tuple

        """
        (addr, off) = self.uint64_decoder(_bytes)
        return (Addr(addr), off)

    def addr_encoder(self, _out, _val):
        """encode Addr to bytes

        :param _out: output list of byte arrays
        :param _val: Addr to encode
        :returns: 'Addr'
        :rtype: string

        """
        self.uint64_encoder(_out, _val._address)
        return 'Addr'
                
    def uint64_decoder(self, _bytes):
        """decode uint64_t

        :param _bytes: Raw bytes
        :returns: tuple of the form (decoded return type, bytes to shift)
        :rtype: tuple

        """
        return (int.from_bytes(bytes(_bytes[0:8]), byteorder='little',
                               signed=False), 8)

    def uint64_encoder(self, _out, _val):
        """encode uint64_t to bytes

        :param _out: output list of byte arrays
        :param _val: integer to encode
        :returns: 'uint64_t'
        :rtype: string

        """
        _out.append(_val.to_bytes(8, byteorder='little'))
        return 'uint64_t'

    def encode(self, _out, _val):
        """Top level encode function.

        :param _out: A list of byte arrays. The caller needs to
             flatten it to a byte stream if they so wish
        :param _val: Value to encode
        :returns: encoded string of the type name
        :rtype: string

        """
        type_name = _val.__class__.__name__
        assert type_name in self._encoders, \
            'No encoder present for type_name - %s' %(type_name)
        
        return self._encoders[type_name](_out, _val)
            
    def decode(self, type_name, _bytes):
        """Top level decode function.

        :param type_name: top level type name
        :param _bytes: Raw bytes
        :returns: tuple of the form (decoded return type, bytes to shift)
        :rtype: tuple

        """
        if '<' in type_name:
            (head, subtypes) = self.get_subtypes(type_name)
            assert head in self._decoders, \
                "No decoders present for type - %s." %(head)
                   
            return self._decoders[head](_bytes, subtypes)
        else:
            assert type_name in self._decoders, \
                "No decoders present for type - %s." %(type_name)
                   
            return self._decoders[type_name](_bytes)
        

# The global serializer instance. User can use this to register new
# encoders/decoders.
serializer = Serialization()

# Do we even need this?
def uuidToBytes(uuid):
    """uuid to bytes in little-endian.

    :param uuid: 
    :returns: bytes in little endian form
    :rtype: bytes

    """
    if uuid is None:
        return b''

    return uuid.bytes

def uuidFromBytes(b):
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

class GTIRBEncoder(json.JSONEncoder):
    """
    A class used to encode GTIRB types for json.
    """
    def default(self, obj):
        if (isinstance(obj, Factory) or
            isinstance(obj, Module) or
            isinstance(obj, IR) or
            isinstance(obj, ProxyBlock) or
            isinstance(obj, AuxDataContainer) or
            isinstance(obj, Block) or
            isinstance(obj, Region) or
            isinstance(obj, ByteMap) or
            isinstance(obj, EdgeLabel) or
            isinstance(obj, Edge) or
            isinstance(obj, CFG) or
            isinstance(obj, DataObject) or
            isinstance(obj, ImageByteMap) or
            isinstance(obj, InstructionRef) or
            isinstance(obj, Section) or
            isinstance(obj, SymStackConst) or
            isinstance(obj, SymAddrConst) or
            isinstance(obj, SymAddrAddr) or
            isinstance(obj, SymbolicExpression) or
            isinstance(obj, Symbol)):
            return obj.__dict__
        elif isinstance(obj, AuxData):
            obj._data = self.default(obj._data)
            return  obj.__dict__
        elif isinstance(obj, dict):
            final = {}
            for k,v in obj.items():
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
    Addr class
    """
    def __init__(self, address):
        self._address = address

        
class Module(object):
    '''
    bytes uuid = 1;
    string binary_path = 2;
    uint64 preferred_addr = 3;
    int64 rebase_delta = 4;
    FileFormat file_format = 5;
    ISAID isa_id = 6;
    string name = 7;
    ImageByteMap image_byte_map = 8;
    repeated Symbol symbols = 9;
    CFG cfg = 10;
    repeated Block blocks = 15;
    repeated DataObject data = 11;
    repeated ProxyBlock proxies = 16;
    repeated Section sections = 12;
    map<uint64, SymbolicExpression> symbolic_operands = 13;
    AuxDataContainer aux_data_container = 14;
    '''
    def __init__(self, uuid, binary_path, preferred_addr, rebase_delta,
                 file_format, isa_id, name, image_byte_map,
                 symbols, cfg, blocks, data, proxies, sections,
                 symbolic_operands, aux_data_container):
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

    def __repr__(self):
        return str(self.__dict__)

    def getAuxData(self, name):
        assert name in self._aux_data_container._aux_data
        return self._aux_data_container._aux_data[name]._data

    def toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = Module_pb2.Module()
        ret.uuid = uuidToBytes(self._uuid)

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
        for k,v in self._symbolic_operands.items():
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
        uuid = uuidFromBytes(_module.uuid)
        module = _factory.objectForUuid(uuid)
        if module is None:
            module = cls(
                uuid, _module.binary_path, _module.preferred_addr,
                _module.rebase_delta, _module.file_format,
                _module.isa_id, _module.name,
                ImageByteMap.fromProtobuf(_factory, _module.image_byte_map),
                [Symbol.fromProtobuf(_factory, sym) for sym in _module.symbols],
                CFG.fromProtobuf(_factory, _module.cfg),
                [Block.fromProtobuf(_factory, blk) for blk in _module.blocks],
                [DataObject.fromProtobuf(_factory, dt) for dt in _module.data],
                [ProxyBlock.fromProtobuf(_factory, pb) for pb in _module.proxies],
                [Section.fromProtobuf(_factory, sec) for sec in _module.sections],
                {key:SymbolicExpression.fromProtobuf(_factory, se)
                 for key,se in _module.symbolic_operands.items()},
                AuxDataContainer.fromProtobuf(_factory,
                                              _module.aux_data_container))
        
            _factory.addObject(uuid, module)
            
        return module

    def removeBlocks(self, blocks_to_remove):
        for block_to_remove in blocks_to_remove:
            self._blocks.remove(block_to_remove)

class IR(object):
    '''
    bytes uuid = 1;
    repeated Module modules = 3;
    AuxDataContainer aux_data_container = 4;
    '''
    def __init__(self, uuid, modules = [], aux_data_container = None):
        self._uuid = uuid
        self._modules = modules
        self._aux_data_container = aux_data_container

    def toProtobuf(self):
        """Returns protobuf representation of the object
    
        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = IR_pb2.IR()
        ret.uuid = uuidToBytes(self._uuid)
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
        uuid = uuidFromBytes(_ir.uuid)
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
    bytes uuid = 1;
    '''
    def __init__(self, uuid):
        self._uuid = uuid

    def toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = ProxyBlock_pb2.ProxyBlock()
        ret.uuid = uuidToBytes(self._uuid)
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
        uuid = uuidFromBytes(_pb.uuid)
        pb = _factory.objectForUuid(uuid)
        if pb is None:
            pb = cls(uuid)
            _factory.addObject(uuid, pb)

        return pb

        
class AuxDataContainer(object):
    '''
    map<string, AuxData> aux_data = 1;
    '''
    def __init__(self, aux_data = {}):
        self._aux_data = aux_data

    def toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = AuxDataContainer_pb2.AuxDataContainer()
        for k,v in self._aux_data.items():
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
        return cls(
            {key:AuxData.fromProtobuf(_factory, val)
             for (key, val) in _aux_data_container.aux_data.items()})
    
class AuxData(object):
    '''
    string type_name = 1;
    bytes data = 2;
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
                # TODO: This is the case when there is an empty data
                # structure somewhere in the aux data tree.
                raise NotImplementedError("Don't handle encoding empty"
                                          "aux  data containers on pygtirb yes")
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
        ret = serializer.decode(_aux_data.type_name,
                          bytearray(_aux_data.data))

        return cls(_aux_data.type_name, ret[0])

class Block(object):
    '''
    bytes uuid = 1;
    uint64 address = 2;
    uint64 size = 3;
    uint64 decode_mode = 4;
    '''
    def __init__(self, uuid, address, size, decode_mode):
        self._uuid = uuid
        self._address = address
        self._size = size
        self._decode_mode = decode_mode

    def toProtobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = Block_pb2.Block()
        ret.uuid = uuidToBytes(self._uuid)
        ret.address = self._address
        ret.size = self._size
        ret.decode_mode = self._decode_mode
        return ret
    
    @classmethod
    def fromProtobuf(cls, _factory, _block):
        """
        Load pygtirb class from protobuf class
        """
        uuid = uuidFromBytes(_block.uuid)
        block = _factory.objectForUuid(uuid)
        if block is None:
            block = cls(uuid, _block.address, _block.size, _block.decode_mode)
            _factory.addObject(uuid, block)

        return block

    def __hash__(self):
        return hash(str(self))
    
    def __repr__(self):
        return str(self.__dict__)
    
    def __eq__(self, other):
        return self.__dict__ == other.__dict__
 
class Region(object):
    '''
    uint64 address = 1;
    bytes data = 2;
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
    repeated Region regions = 1;
    '''
    def __init__(self, regions = []):
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
        Load this class from protobuf object
        """
        return cls(
            [Region.fromProtobuf(_factory, region)
             for region in _byte_map.regions])
    
    
class EdgeLabel(object):
    '''
    bool conditional = 1;
    bool direct = 2;
    EdgeType type = 3;
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
        ret.type = self._type
        return ret
        
    @classmethod
    def fromProtobuf(cls, _factory, _edge_label):
        """
        Load this class from protobuf object
        """
        return cls(
            _edge_label.conditional, _edge_label.direct, _edge_label.type)
    
class Edge(object):
    '''
    reserved 3, 4;
    reserved "boolean", "integer";

    bytes source_uuid = 1;
    bytes target_uuid = 2;
    EdgeLabel label = 5;
    '''
    def __init__(self, source_uuid, target_uuid, label,
                 source_block=None, target_block=None):
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
        ret.source_uuid = uuidToBytes(self._source_uuid)
        ret.target_uuid = uuidToBytes(self._target_uuid)
        ret.label.CopyFrom(self._label.toProtobuf())
        return ret
           
    @classmethod
    def fromProtobuf(cls, _factory, _edge):
        """
        Load this class from protobuf object
        """
        return cls(uuidFromBytes(_edge.source_uuid),
                   uuidFromBytes(_edge.target_uuid),
                   EdgeLabel.fromProtobuf(_factory, _edge.label))

    def __hash__(self):
        return hash(str(self))
    
    def __repr__(self):
        return str(self.__dict__)
    
    def __eq__(self, other):
        return self.__dict__ == other.__dict__

    
class CFG(object):
    '''
    reserved 1;
    reserved "blocks";

    repeated bytes vertices = 3;
    repeated Edge edges = 2;
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
        Load this class from protobuf object
        """
        return cls(_cfg.vertices,
                   [Edge.fromProtobuf(_factory, e) for e in
    _cfg.edges])
    
    def removeEdges(self, edges_to_remove):
        for edge_to_remove in edges_to_remove:
            self._edges.remove(edge_to_remove)

            
class DataObject(object):
    '''
    bytes uuid = 1;
    uint64 address = 2;
    uint64 size = 3;
    '''
    def __init__(self, uuid, address, size):
        self._uuid = uuid
        self._address = address
        self._size = size

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = DataObject_pb2.DataObject()
        ret.uuid = uuidToBytes(self._uuid)
        ret.address = self._address
        ret.size = self._size
        return ret
        
    @classmethod
    def fromProtobuf(cls, _factory, _data_object):
        """
        Load this class from protobuf object
        """
        uuid = uuidFromBytes(_data_object.uuid)
        data_object = _factory.objectForUuid(uuid)
        if data_object is None:
            data_object = cls(uuid, _data_object.address, _data_object.size)
            _factory.addObject(uuid, data_object)

        return data_object
    
        
class ImageByteMap(object):
    '''
    bytes uuid = 1;
    ByteMap byte_map = 2;
    uint64 addr_min = 3;
    uint64 addr_max = 4;
    uint64 base_address = 5;
    uint64 entry_point_address = 6;
    '''

    def __init__(self, uuid, byte_map, addr_min, addr_max,
                 base_address, entry_point_address):
        self._uuid = uuid
        self._byte_map = byte_map
        self._addr_min = addr_min
        self._addr_max = addr_max
        self._base_address = base_address
        self._entry_point_address = entry_point_address

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = ImageByteMap_pb2.ImageByteMap()
        ret.uuid = uuidToBytes(self._uuid)
        ret.byte_map.CopyFrom(self._byte_map.toProtobuf())
        ret.addr_min = self._addr_min
        ret.addr_max = self._addr_max
        ret.base_address = self._base_address
        ret.entry_point_address = self._entry_point_address
        return ret
        
    @classmethod
    def fromProtobuf(cls, _factory, _image_byte_map):
        """
        Load this class from protobuf object
        """
        uuid = uuidFromBytes(_image_byte_map.uuid)
        image_byte_map = _factory.objectForUuid(uuid)
        if image_byte_map is None:
            image_byte_map = cls(
                uuid, ByteMap.fromProtobuf(_factory, _image_byte_map.byte_map),
                _image_byte_map.addr_min, _image_byte_map.addr_max,
                _image_byte_map.base_address,
                _image_byte_map.entry_point_address)
            _factory.addObject(uuid, image_byte_map)
            
        return image_byte_map
        
class InstructionRef(object):
    '''
    bytes block_id = 1;
    uint64 offset = 2;
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
        Load this class from protobuf object
        """
        return cls(_instruction_ref.block_id, _instruction_ref.offset)

class Section(object):
    '''
    bytes uuid = 1;
    string name = 2;
    uint64 address = 3;
    uint64 size = 4;
    '''
    def __init__(self, uuid, name, address, size):
        self._uuid = uuid
        self._name = name
        self._address = address
        self._size = size

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = Section_pb2.Section()
        ret.uuid = uuidToBytes(self._uuid)
        ret.name = self._name
        ret.address = self._address
        ret.size = self._size
        return ret

    @classmethod
    def fromProtobuf(cls, _factory, _section):
        """
        Load this class from protobuf object
        """
        uuid = uuidFromBytes(_section.uuid)
        section = _factory.objectForUuid(uuid)
        if section is None:
            section = cls(uuid, _section.name, _section.address, _section.size)
            _factory.addObject(uuid, section)

        return section

class SymStackConst(object):
    '''
    int32 offset = 1;
    bytes symbol_uuid = 2;
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
        ret.symbol_uuid = uuidToBytes(self._symbol_uuid)
        return ret
        
    @classmethod
    def fromProtobuf(cls, _factory, _sym_stack_const):
        """
        Load this class from protobuf object
        """
        if _sym_stack_const.symbol_uuid != b'':
            return cls(_sym_stack_const.offset,
                       uuidFromBytes(_sym_stack_const.symbol_uuid))
        else:
            return cls(_sym_stack_const.offset)
    

class SymAddrConst(object):
    '''
    int64 offset = 1;
    bytes symbol_uuid = 2;
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
        ret.symbol_uuid = uuidToBytes(self._symbol_uuid)
        return ret
        
    @classmethod
    def fromProtobuf(cls, _factory, _sym_addr_const):
        """
        Load this class from protobuf object
        """
        return cls(_sym_addr_const.offset,
                   uuidFromBytes(_sym_addr_const.symbol_uuid))
        
class SymAddrAddr(object):
    '''
    int64 scale = 1;
    int64 offset = 2;
    bytes symbol1_uuid = 3;
    bytes symbol2_uuid = 4;
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
        ret.symbol1_uuid = uuidToBytes(self._symbol1_uuid)
        ret.symbol2_uuid = uuidToBytes(self._symbol2_uuid)
        return ret
    
    @classmethod
    def fromProtobuf(cls, _factory, _sym_addr_addr):
        """
        Load this class from protobuf object
        """
        return cls(_sym_addr_addr.scale, _sym_addr_addr.offset,
                   uuidFromBytes(_sym_addr_addr.symbol1_uuid),
                   uuidFromBytes(_sym_addr_addr.symbol2_uuid))
        
class SymbolicExpression(object):
    '''
    oneof value {
        SymStackConst stack_const = 1;
        SymAddrConst addr_const = 2;
        SymAddrAddr addr_addr = 3;
    }
    '''

    def __init__(self, stack_const = None, addr_const = None,
                 addr_addr = None):
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
        Load this class from protobuf object
        """
        stack_const = None
        addr_const = None
        addr_addr = None
        
        if _symbolic_expression.HasField('stack_const'):
            stack_const = getattr(_symbolic_expression, 'stack_const',
                                  None)
        if _symbolic_expression.HasField('addr_const'):
            addr_const = getattr(_symbolic_expression, 'addr_const', None)

        if _symbolic_expression.HasField('addr_addr'):
            addr_addr = getattr(_symbolic_expression, 'addr_addr', None)

        return cls(
            stack_const = (SymStackConst.fromProtobuf(_factory, stack_const)
                           if stack_const is not None else None),
            addr_const = (SymAddrConst.fromProtobuf(_factory, addr_const)
                          if addr_const is not None else None),
            addr_addr = (SymAddrAddr.fromProtobuf(_factory, addr_addr)
                         if addr_addr is not None else None)
            )
        
class Symbol(object):
    '''
    bytes uuid = 1;
    oneof optional_payload {
      uint64 value = 2;
      bytes referent_uuid = 5;
    }
    string name = 3;
    StorageKind storage_kind = 4;
    '''
    def __init__(self, uuid, name, storage_kind,
                 value = None, referent_uuid = None, referent = None):
        self._uuid = uuid
        self._value = value
        self._referent_uuid = referent_uuid
        self._referent = referent
        self._name = name
        self._storage_kind = storage_kind

    def toProtobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = Symbol_pb2.Symbol()
        ret.uuid = uuidToBytes(self._uuid)

        if self._value is not None:
            ret.value = self._value

        if self._referent_uuid is not None:
            ret.referent_uuid = uuidToBytes(self._referent_uuid)
            
        ret.name = self._name
        ret.storage_kind = self._storage_kind
        return ret
        
    @classmethod
    def fromProtobuf(cls, _factory, _symbol):
        """
        Load this class from protobuf object
        """
        uuid = uuidFromBytes(_symbol.uuid)
        symbol = _factory.objectForUuid(uuid)
        if symbol is None:
            value = None
            referent_uuid = None

            if _symbol.HasField('value'):
                value = getattr(_symbol, 'value')
           
            if _symbol.HasField('referent_uuid'):
                referent_uuid = uuidFromBytes(
                    getattr(_symbol, 'referent_uuid'))

            symbol = cls(uuid, _symbol.name, _symbol.storage_kind,
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

if __name__== "__main__":
    main()
