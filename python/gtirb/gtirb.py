import sys
sys.path.append("../../src/proto/")

import IR_pb2
import json
import uuid

def uuidFromBytes(b):
    return uuid.UUID(bytes=b)

def custom_default(obj):
    """
    A custom `default` function used to dump json.
    :param obj: any object seen when walking the json object.
    :return: json representation of the object
    """
    if (isinstance(obj, Factory) or
        isinstance(obj, Module) or
        isinstance(obj, IR) or
        isinstance(obj, ProxyBlock) or
        isinstance(obj, AuxDataContainer) or
        isinstance(obj, AuxData) or
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
    
                                                                            
class Factory(object):
    def __init__(self):
        self._objects = {}

    def objectForUuid(self, uuid):
        return self._objects.get(uuid, None)

    def addObject(self, uuid, obj):
        assert uuid not in self._objects
        self._objects[uuid] = obj
    
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
    def fromProtobuf(cls, _factory, _module):
        uuid = uuidFromBytes(_module.uuid)
        module = _factory.objectForUuid(uuid)
        if module is None:
            module = cls(
                uuid, _module.binary_path, _module.preferred_addr,
                _module.rebase_delta, _module.file_format,
                _module.isa_id, _module.name,
                ImageByteMap.fromProtobuf(_factory, _module.image_byte_map),
                [Symbol.fromProtobuf(_factory, sym) for sym in _module.symbols],
                _module.cfg,
                [Block.fromProtobuf(_factory, blk) for blk in _module.blocks],
                _module.data,
                [ProxyBlock.fromProtobuf(_factory, pb) for pb in _module.proxies],
                [Section.fromProtobuf(_factory, sec) for sec in _module.sections],
                {key:SymbolicExpression.fromProtobuf(_factory, se)
                 for key,se in _module.symbolic_operands.items()},
                AuxDataContainer.fromProtobuf(_factory,
                                              _module.aux_data_container))
        
            _factory.addObject(uuid, module)
            
        return module

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
           
    @classmethod
    def fromProtobuf(cls, _factory, _ir):
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

        
class ProxyBlock(object):
    def __init__(self, uuid):
        self._uuid = uuid

           
    @classmethod
    def fromProtobuf(cls, _factory, _pb):
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

           
    @classmethod
    def fromProtobuf(cls, _factory, _aux_data_container):
        return cls(
            {key:AuxData.fromProtobuf(_factory, val)
             for (key, val) in _aux_data_container.aux_data.items()})
    
class AuxData(object):
    '''
    string type_name = 1;
    bytes data = 2;
    '''
    def __init__(self, type_name, data):
        self._type_name = type_name
        self._data = data

           
    @classmethod
    def fromProtobuf(cls, _factory, _aux_data):
        # TODO: CRITICAL: Implement different kinds here.
        return cls(None, None)

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

           
    @classmethod
    def fromProtobuf(cls, _factory, _block):
        uuid = uuidFromBytes(_block.uuid)
        block = _factory.objectForUuid(uuid)
        if block is None:
            block = cls(uuid, _block.address, _block.size, _block.decode_mode)
            _factory.addObject(uuid, block)

        return block
            
class Region(object):
    '''
    uint64 address = 1;
    bytes data = 2;
    '''
    def __init__(self, address, data):
        self._address = address
        self._data = data

           
    @classmethod
    def fromProtobuf(cls, _factory, _region):
        return cls(_region.address, _region.data)

            
class ByteMap(object):
    '''
    repeated Region regions = 1;
    '''
    def __init__(self, regions = []):
        self._regions = regions

           
    @classmethod
    def fromProtobuf(cls, _factory, _byte_map):
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

           
    @classmethod
    def fromProtobuf(cls, _factory, _edge_label):
        return cls(
            _edge_label.conditonal, _edge_label.direct, _edge_label.type)
    
class Edge(object):
    '''
    reserved 3, 4;
    reserved "boolean", "integer";

    bytes source_uuid = 1;
    bytes target_uuid = 2;
    EdgeLabel label = 5;
    '''
    def __init__(self, source_uuid, target_uuid, label):
        self._source_uuid = source_uuid
        self._target_uuid = target_uuid
        self._label = label

           
    @classmethod
    def fromProtobuf(cls, _factory, _edge):
        return cls(uuidFromBytes(_edge.source_uuid),
                   uuidFromBytes(_edge.target_uuid),
                   EdgeLabel.fromProtobuf(_factory, _edge.label))

        
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

           
    @classmethod
    def fromProtobuf(cls, _factory, _cfg):
        return cls(_cfg.vertices,
                   [Edge.fromProtobuf(_factory, e) for e in _cfg.edges])

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

           
    @classmethod
    def fromProtobuf(cls, _factory, _data_object):
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

           
    @classmethod
    def fromProtobuf(cls, _factory, _image_byte_map):
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

           
    @classmethod
    def fromProtobuf(cls, _factory, _instruction_ref):
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

           
    @classmethod
    def fromProtobuf(cls, _factory, _section):
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
    def __init__(self, offset, symbol_uuid):
        self._offset = offset
        self._symbol_uuid = symbol_uuid

           
    @classmethod
    def fromProtobuf(cls, _factory, _sym_stack_const):
        return cls(_sym_stack_const.offset,
                   uuidFromBytes(_sym_stack_const.symbol_uuid))
    

class SymAddrConst(object):
    '''
    int64 offset = 1;
    bytes symbol_uuid = 2;
    '''
    def __init__(self, offset, symbol_uuid):
        self._offset = offset
        self._symbol_uuid = symbol_uuid

           
    @classmethod
    def fromProtobuf(cls, _factory, _sym_addr_const):
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

           
    @classmethod
    def fromProtobuf(cls, _factory, _sym_addr_addr):
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

    def __init__(self, _stack_const = None, _addr_const = None,
                 _addr_addr = None):
        self._stack_const = stack_const
        self._addr_const = addr_const
        self._addr_addr = addr_addr

           
    @classmethod
    def fromProtobuf(cls, _factory, _symbolic_expression):
        out = None

        field_name = _symbolic_expression.WhichOneOf("value")
        if field_name is not None:
            if field_name == 'stack_const':
                out = cls(stack_const = SymStackConst.fromProtobuf(
                    _factory, _symbolic_expression.stack_const))
            if field_name == 'addr_const':
                out = cls(addr_const = SymAddrConst.fromProtobuf(
                    _symbolic_expression.addr_const))
            if field_name == 'addr_addr':
                out = cls(addr_addr = SymAddrAddr.fromProtobuf(
                    _symbolic_expression.addr_addr))
        else:
            out = cls()

        assert out is not None
        return out
            
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
                 value = None, referent_uuid = None):
        self._uuid = uuid
        self._value = value
        self._referent_uuid = referent_uuid
        self._name = name
        self._storage_kind = storage_kind
                
    @classmethod
    def fromProtobuf(cls, _factory, _symbol):
        uuid = uuidFromBytes(_symbol.uuid)
        symbol = _factory.objectForUuid(uuid)
        if symbol is None:
            value = None
            referent_uuid = None

            try:
                value = _symbol.value
            except Exception as ex:
                pass
           
            try:
                referent_uuid = uuidFromBytes(_symbol.referent_uuid)
            except Exception as  ex:
                pass
           

            symbol = cls(uuid, _symbol.name, _symbol.storage_kind,
                         value, referent_uuid)
            _factory.addObject(uuid, symbol)

        return symbol
        
def main():
    with open('foo.gtirb', 'rb') as f:
        _ir = IR_pb2.IR()
        _ir.ParseFromString(f.read())
        
        factory = Factory()
        ir = IR.fromProtobuf(factory, _ir)

        print(_ir)
        print(ir)
        print(json.dumps(ir, indent=2, default=custom_default))            
    
if __name__== "__main__":
    main()
