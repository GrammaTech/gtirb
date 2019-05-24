# -*- coding: utf-8 -*-
"""
The GTIRB python serialization module.

Contains the `Serialization` class that is responsible for
encoding/decoding various GTIRB types to/from bytes and the `Codec`
class that holds the encode and decode functions for a type.

"""
import gtirb
import io
import sys
import uuid


class Codec(object):
    '''
    A class that holds the `encode` and `decode` methods for a type.
    '''

    def __init__(self):
        pass


class MappingCodec(Codec):
    def decode(self, _bytes, _sub_types, _serialization):
        """decode a mapping<..> entry
    
        :param _bytes: Raw bytes (io.BytesIO typed)
        :param _sub_types: List of subtype names
        :param _serialization: The Serializaton instance calling this
        :returns: decoded return type
        :rtype: tuple
    
        """
        assert len(_sub_types) == 2

        _key_type = _sub_types[0]
        _value_type = _sub_types[1]
        _ret = {}

        size = _serialization.decode('uint64_t', _bytes)

        for _ in range(0, size):
            k = _serialization.decode(_key_type, _bytes)
            v = _serialization.decode(_value_type, _bytes)
            _ret[k] = v

        return _ret

    def encode(self, _out, _map, _serialization):
        """encode a dict into bytes.
    
        :param _out: output list of byte arrays.
        :param _map: map to encode
        :param _serialization: The Serialization instance calling this
        :returns: the encoded type_name string
        :rtype: string
    
        """
        _serialization.encode(_out, len(_map))
        k_string = ''
        v_string = ''

        for k, v in _map.items():
            if k_string == '':
                k_string = _serialization.encode(_out, k)
            else:
                assert k_string == _serialization.encode(_out, k),\
                    "keys with different types present in this map"

            if v_string == '':
                v_string = _serialization.encode(_out, v)
            else:
                assert v_string == _serialization.encode(_out, v),\
                    "values with different types present in this map"

        return 'mapping<%s,%s>' % (k_string, v_string)


class SetCodec(Codec):
    def decode(self, _bytes, _sub_types, _serialization):
        """decode a set<..> entry
    
        :param _bytes: Raw bytes (io.BytesIO typed)
        :param _sub_types: a list of sub-type names
        :param _serialization: A Serialization instance calling this
        :returns: decoded return type
        :rtype: tuple
    
        """
        assert len(_sub_types) == 1

        _type = _sub_types[0]
        _ret = set()

        size = _serialization.decode('uint64_t', _bytes)

        for index in range(0, size):
            v = _serialization.decode(_type, _bytes)
            _ret.add(v)

        return _ret

    def encode(self, _out, _set, _serialization):
        """encode a set() to bytes
    
        :param _out: output list of byte arrays
        :param _set: set to encode
        :param _serialization: The Serialization instance calling this
        :returns: encoded type name string
        :rtype: string
    
        """
        _serialization.encode(_out, len(_set))
        type_string = ''
        for item in _set:
            if type_string == '':
                type_string = _serialization.encode(_out, item)
            else:
                assert type_string == _serialization.encode(_out, item),\
                     "values with different types present in this set"

        return 'set<%s>' % (type_string)


class SequenceCodec(Codec):
    def decode(self, _bytes, _sub_types, _serialization):
        """decode a sequence<..> entry
        
        :param _bytes: Raw bytes (io.BytesIO typed)
        :param _sub_types: a list of sub-type names
        :param _serialization: The Serialization instance calling this
        :returns: decoded return type
        :rtype: tuple
    
        """
        assert len(_sub_types) == 1

        _type = _sub_types[0]
        _ret = []

        size = _serialization.decode('uint64_t', _bytes)

        for index in range(0, size):
            v = _serialization.decode(_type, _bytes)
            _ret.append(v)

        return _ret

    def encode(self, _out, _list, _serialization):
        """encode a list to bytes
    
        :param _out: list of byte arrays
        :param _list: list to encode
        :param _serialization: The Serialization instance calling this
        :returns: encoded type name string
        :rtype: string
    
        """
        _serialization.encode(_out, len(_list))
        type_string = ''
        for item in _list:
            if type_string == '':
                type_string = _serialization.encode(_out, item)
            else:
                assert type_string == _serialization.encode(_out, item),\
                    "values with different types present in this sequence"

        return 'sequence<%s>' % (type_string)


class StringCodec(Codec):
    def decode(self, _bytes, _serialization):
        """decode a string
    
        :param _bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple
    
        """
        size = _serialization.decode('uint64_t', _bytes)

        return str(_bytes.read(size), 'utf-8')

    def encode(self, _out, _val, _serialization=None):
        """encode a string to bytes
    
        :param _out: output list of byte arrays
        :param _val: string to encode
        :returns: "string"
        :rtype: string
    
        """
        _serialization.encode(_out, len(_val))
        _out.append(_val.encode())
        return 'string'


class IrefCodec(Codec):
    def decode(self, _bytes, _serialization):
        """decode an InstructionRef entry
    
        :param _bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple
    
        """
        bid = _serializtion.decode('UUID', _bytes)
        offset = _serialization.decode('uint64_t', _bytes)

        return InstructionRef(bid, offset)

    def encode(self, _out, _val, _serialization=None):
        """
        encode InstructionRef to bytes
        """

        _serialization.encode(_out, _val._block_id)
        _serialization.encode(_out, _val._offset)
        return 'InstructionRef'


class UUIDCodec(Codec):
    def decode(self, _bytes, _serialization):
        """decode a UUID entry
    
        :param _bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple
    
        """
        if sys.getsizeof(_bytes) < 16:
            return None
        return uuid.UUID(bytes=_bytes.read(16))

    def encode(self, _out, _val, _serialization=None):
        """encode UUID to bytes
    
        :param _out: output list of byte arrays
        :param _val: uuid to encode
        :returns: "UUID"
        :rtype: string
    
        """
        _out.append(_val.bytes)
        return 'UUID'


class AddrCodec(Codec):
    def decode(self, _bytes, _serialization):
        """decode an InstructionRef entry
    
        :param _bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple
    
        """
        addr = _serialization.decode('uint64_t', _bytes)
        return gtirb.Addr(addr)

    def encode(self, _out, _val, _serialization=None):
        """encode Addr to bytes
    
        :param _out: output list of byte arrays
        :param _val: Addr to encode
        :returns: 'Addr'
        :rtype: string
    
        """
        _serialization.encode(_out, _val._address)
        return 'Addr'


class Uint64Codec(Codec):
    def decode(self, _bytes, _serialization):
        """decode uint64_t
    
        :param _bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple
    
        """
        return int.from_bytes(_bytes.read(8),
                              byteorder='little',
                              signed=False)
    
    def encode(self, _out, _val, _serialization=None):
        """encode uint64_t to bytes
    
        :param _out: output list of byte arrays
        :param _val: integer to encode
        :returns: 'uint64_t'
        :rtype: string
    
        """
        _out.append(_val.to_bytes(8, byteorder='little'))
        return 'uint64_t'


class Serialization(object):
    """
    A class used to encode/decode aux data table entries. Use the top
    level `register_codec`, `encode` and `decode` functions.
    """

    def _getSubtypes(self, type_name):
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
                    subtypes.append(type_name[last_index + 1:index])
                    break
                else:
                    depth -= 1
            elif c == ',':
                if depth == 1:
                    assert last_index != -1
                    subtypes.append(type_name[last_index + 1:index])
                    last_index = index

            index += 1

        return (head, subtypes)

    def _getEncodedTypeMapping(self, type_name):
        return self._type_mapping.get(type_name, type_name)

    def __init__(self):
        """
        Initializes all encoders and decoders.
        """

        self._codecs = {
            'Addr': AddrCodec(),
            'InstructionRef': IrefCodec(),
            'mapping': MappingCodec(),
            'sequence': SequenceCodec(),
            'set': SetCodec(),
            'string': StringCodec(),
            'uint64_t': Uint64Codec(),
            'UUID': UUIDCodec()
        }
        # Some special type mappings from python to GTIR encoded types.
        self._type_mapping = {
            'dict': 'mapping',
            'int': 'uint64_t',
            'list': 'sequence',
            'set': 'set',
            'str': 'string'
        }

    def registerCodec(self, type_name, codec):
        """Register a Codec for a custom type. Use this method to
        register encode/decode functions for your own classes

        :param type_name: string type_name, this should be same as the
            __class__.__name__ field of the class
        :param codec: A Codec instance

        """
        assert type_name not in self._codecs, \
            'Type - %s already has a codec' %(type_name)

        self._codecs[type_name] = codec

    def encode(self, _out, _val):
        """Top level encode function.

        :param _out: A list of byte arrays. The caller needs to
             flatten it to a byte stream if they so wish
        :param _val: Value to encode
        :returns: encoded string of the type name
        :rtype: string

        """
        type_name = self._getEncodedTypeMapping(_val.__class__.__name__)
        assert type_name in self._codecs, \
            'No encoder present for type_name - %s' %(type_name)

        return self._codecs[type_name].encode(_out, _val, self)

    def decode(self, type_name, _bytes):
        """Top level decode function.

        :param type_name: top level type name
        :param _bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        assert isinstance(_bytes, io.BytesIO)
        if '<' in type_name:
            (head, subtypes) = self._getSubtypes(type_name)
            assert head in self._codecs, \
                "No decoders present for type - %s." %(head)

            return self._codecs[head].decode(_bytes, subtypes, self)
        else:
            assert type_name in self._codecs, \
                "No decoders present for type - %s." %(type_name)

            return self._codecs[type_name].decode(_bytes, _serialization=self)
