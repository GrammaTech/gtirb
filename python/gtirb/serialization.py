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

        key_type = _sub_types[0]
        value_type = _sub_types[1]
        ret = {}

        size = _serialization.decode('uint64_t', _bytes)

        for _ in range(0, size):
            k = _serialization.decode(key_type, _bytes)
            v = _serialization.decode(value_type, _bytes)
            ret[k] = v

        return ret

    def encode(self, _out, _map, _serialization, *, _type_name_hint=''):
        """encode a dict into bytes.
    
        :param _out: output bytes in io.BytesIO type
        :param _map: map to encode
        :param _serialization: The Serialization instance calling this
        :returns: the encoded type_name string
        :rtype: string
    
        """
        if len(_map) == 0:
            if not _type_name_hint:
                raise NotImplementedError(
                    "Cannot encode an empty map with no type hint")

        k_string = ''
        v_string = ''

        if _type_name_hint:
            assert '<' in _type_name_hint
            (head, subtypes) = _serialization._getSubtypes(_type_name_hint)
            k_string = subtypes[0]
            v_string = subtypes[1]
            assert head == 'mapping'

        _serialization.encode(_out, len(_map))

        for k, v in _map.items():
            if k_string == '':
                k_string = _serialization.encode(_out,
                                                 k,
                                                 _type_name_hint=k_string)
            else:
                assert k_string == _serialization.encode(_out, k, _type_name_hint=k_string),\
                    "keys with different types present in this map"

            if v_string == '':
                v_string = _serialization.encode(_out,
                                                 v,
                                                 _type_name_hint=v_string)
            else:
                assert v_string == _serialization.encode(_out, v, _type_name_hint=v_string),\
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

        type = _sub_types[0]
        ret = set()

        size = _serialization.decode('uint64_t', _bytes)

        for index in range(0, size):
            v = _serialization.decode(type, _bytes)
            ret.add(v)

        return ret

    def encode(self, _out, _set, _serialization, *, _type_name_hint=''):
        """encode a set() to bytes
    
        :param _out: output list of byte arrays
        :param _set: set to encode
        :param _serialization: The Serialization instance calling this
        :returns: encoded type name string
        :rtype: string
    
        """
        if len(_set) == 0:
            if not _type_name_hint:
                raise NotImplementedError(
                    "Cannot encode an empty set with no type hint")

        type_string = ''

        if _type_name_hint:
            assert '<' in _type_name_hint
            (head, subtypes) = _serialization._getSubtypes(_type_name_hint)
            type_string = subtypes[0]
            assert head == 'set'

        _serialization.encode(_out, len(_set))

        for item in _set:
            if type_string == '':
                type_string = _serialization.encode(
                    _out, item, _type_name_hint=type_string)
            else:
                assert type_string == _serialization.encode(_out, item, _type_name_hint=type_string),\
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

    def encode(self, _out, _list, _serialization, *, _type_name_hint=''):
        """encode a list to bytes
    
        :param _out: list of byte arrays
        :param _list: list to encode
        :param _serialization: The Serialization instance calling this
        :returns: encoded type name string
        :rtype: string
    
        """
        type_string = ''

        if len(_list) == 0:
            if not _type_name_hint:
                raise NotImplementedError(
                    "Cannot encode an empty sequence with no type hint")

        if _type_name_hint:
            assert '<' in _type_name_hint
            (head, subtypes) = _serialization._getSubtypes(_type_name_hint)
            assert head == 'sequence'
            type_string = subtypes[0]

        _serialization.encode(_out, len(_list))

        for item in _list:
            if type_string == '':
                type_string = _serialization.encode(
                    _out, item, _type_name_hint=type_string)
            else:
                assert type_string == _serialization.encode(_out, item, _type_name_hint=type_string),\
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

    def encode(self, _out, _val, _serialization=None, *, _type_name_hint=''):
        """encode a string to bytes
    
        :param _out: output list of byte arrays
        :param _val: string to encode
        :returns: "string"
        :rtype: string
    
        """
        ret = 'string'
        if _type_name_hint:
            assert _type_name_hint == ret

        _serialization.encode(_out, len(_val))
        _out.write(_val.encode())
        return ret


class OffsetCodec(Codec):
    def decode(self, _bytes, _serialization):
        """decode an Offset entry
    
        :param _bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple
    
        """
        elementId = _serialization.decode('UUID', _bytes)
        offset = _serialization.decode('uint64_t', _bytes)

        return Offset(elementId, offset)

    def encode(self, _out, _val, _serialization=None, *, _type_name_hint=''):
        """
        encode Offset to bytes
        """
        ret = 'Offset'
        if _type_name_hint:
            assert _type_name_hint == ret

        _serialization.encode(_out, _val._element_id)
        _serialization.encode(_out, _val._offset)
        return ret


class UUIDCodec(Codec):
    def decode(self, _bytes, _serialization):
        """decode a UUID entry
    
        :param _bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple
    
        """
        return uuid.UUID(bytes=_bytes.read(16))

    def encode(self, _out, _val, _serialization=None, *, _type_name_hint=''):
        """encode UUID to bytes
    
        :param _out: output list of byte arrays
        :param _val: uuid to encode
        :returns: "UUID"
        :rtype: string
    
        """
        ret = 'UUID'
        if _type_name_hint:
            assert _type_name_hint == ret

        _out.write(_val.bytes)
        return ret


class AddrCodec(Codec):
    def decode(self, _bytes, _serialization):
        """decode an Addr entry
    
        :param _bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple
    
        """
        addr = _serialization.decode('uint64_t', _bytes)
        return gtirb.Addr(addr)

    def encode(self, _out, _val, _serialization=None, *, _type_name_hint=''):
        """encode Addr to bytes
    
        :param _out: output list of byte arrays
        :param _val: Addr to encode
        :returns: 'Addr'
        :rtype: string
    
        """
        ret = 'Addr'
        if _type_name_hint:
            assert _type_name_hint == ret

        _serialization.encode(_out, _val._address)
        return ret


class Uint64Codec(Codec):
    def decode(self, _bytes, _serialization):
        """decode uint64_t
    
        :param _bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple
    
        """
        return int.from_bytes(_bytes.read(8), byteorder='little', signed=False)

    def encode(self, _out, _val, _serialization=None, *, _type_name_hint=''):
        """encode uint64_t to bytes
    
        :param _out: output list of byte arrays
        :param _val: integer to encode
        :returns: 'uint64_t'
        :rtype: string
    
        """
        ret = 'uint64_t'
        if _type_name_hint:
            assert _type_name_hint == ret

        _out.write(_val.to_bytes(8, byteorder='little'))
        return ret


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
            'Offset': OffsetCodec(),
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

    def encode(self, _out, _val, *, _type_name_hint=''):
        """Top level encode function.

        :param _out: A list of byte arrays. The caller needs to
             flatten it to a byte stream if they so wish
        :param _val: Value to encode
        :returns: encoded string of the type name
        :rtype: string

        """
        assert isinstance(_out, io.BytesIO)
        type_name = self._getEncodedTypeMapping(_val.__class__.__name__)
        assert type_name in self._codecs, \
            'No encoder present for type_name - %s' %(type_name)

        return self._codecs[type_name].encode(_out,
                                              _val,
                                              self,
                                              _type_name_hint=_type_name_hint)

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
