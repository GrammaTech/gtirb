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


class Codec:
    """
    A class that holds the `encode` and `decode` methods for a type.
    """
    def __init__(self):
        pass


class MappingCodec(Codec):
    def decode(self, bytes, sub_types, serialization):
        """decode a mapping<..> entry

        :param bytes: Raw bytes (io.BytesIO typed)
        :param sub_types: List of subtype names
        :param serialization: The Serializaton instance calling this
        :returns: decoded return type
        :rtype: tuple

        """
        assert len(sub_types) == 2

        key_type, value_type = sub_types

        size = serialization.decode('uint64_t', bytes)
        ret = {}
        for _ in range(size):
            k = serialization.decode(key_type, bytes)
            v = serialization.decode(value_type, bytes)
            ret[k] = v

        return ret

    def encode(self, out, map, serialization, *, type_name_hint=''):
        """encode a dict into bytes.

        :param out: output bytes in io.BytesIO type
        :param map: map to encode
        :param serialization: The Serialization instance calling this
        :returns: the encoded type_name string
        :rtype: string

        """
        if len(map) == 0:
            if not type_name_hint:
                raise NotImplementedError(
                    "Cannot encode an empty map with no type hint"
                )

        k_string = ''
        v_string = ''

        if type_name_hint:
            assert '<' in type_name_hint
            head, (k_string, v_string) = \
                serialization.getSubtypes(type_name_hint)
            assert head == 'mapping'

        serialization.encode(out, len(map))

        for k, v in map.items():
            if k_string == '':
                k_string = serialization.encode(out,
                                                k,
                                                type_name_hint=k_string)
            else:
                assert k_string == serialization.encode(
                    out, k, type_name_hint=k_string
                ), "keys with different types present in this map"

            if v_string == '':
                v_string = serialization.encode(out,
                                                v,
                                                type_name_hint=v_string)
            else:
                assert v_string == serialization.encode(
                    out, v, type_name_hint=v_string
                ), "values with different types present in this map"

        return 'mapping<%s,%s>' % (k_string, v_string)


class SetCodec(Codec):
    def decode(self, bytes, sub_types, serialization):
        """decode a set<..> entry

        :param bytes: Raw bytes (io.BytesIO typed)
        :param sub_types: a list of sub-type names
        :param serialization: A Serialization instance calling this
        :returns: decoded return type
        :rtype: tuple

        """
        assert len(sub_types) == 1

        type = sub_types[0]
        ret = set()

        size = serialization.decode('uint64_t', bytes)

        for _ in range(size):
            v = serialization.decode(type, bytes)
            ret.add(v)

        return ret

    def encode(self, out, set, serialization, *, type_name_hint=''):
        """encode a set() to bytes

        :param out: output list of byte arrays
        :param set: set to encode
        :param serialization: The Serialization instance calling this
        :returns: encoded type name string
        :rtype: string

        """
        if len(set) == 0:
            if not type_name_hint:
                raise NotImplementedError(
                    "Cannot encode an empty set with no type hint")

        type_string = ''

        if type_name_hint:
            assert '<' in type_name_hint
            head, (subtypes, ) = serialization.getSubtypes(type_name_hint)
            assert head == 'set'

        serialization.encode(out, len(set))

        for item in set:
            if type_string == '':
                type_string = serialization.encode(
                    out, item, type_name_hint=type_string)
            else:
                assert type_string == serialization.encode(
                    out, item, type_name_hint=type_string
                ), "values with different types present in this set"

        return 'set<%s>' % (type_string)


class SequenceCodec(Codec):
    def decode(self, bytes, sub_types, serialization):
        """decode a sequence<..> entry

        :param bytes: Raw bytes (io.BytesIO typed)
        :param sub_types: a list of sub-type names
        :param serialization: The Serialization instance calling this
        :returns: decoded return type
        :rtype: tuple

        """
        assert len(sub_types) == 1

        type = sub_types[0]
        ret = []

        size = serialization.decode('uint64_t', bytes)

        for _ in range(size):
            v = serialization.decode(type, bytes)
            ret.append(v)

        return ret

    def encode(self, out, list, serialization, *, type_name_hint=''):
        """encode a list to bytes

        :param out: list of byte arrays
        :param list: list to encode
        :param serialization: The Serialization instance calling this
        :returns: encoded type name string
        :rtype: string

        """
        type_string = ''

        if len(list) == 0:
            if not type_name_hint:
                raise NotImplementedError(
                    "Cannot encode an empty sequence with no type hint")

        if type_name_hint:
            assert '<' in type_name_hint
            head, subtypes = serialization.getSubtypes(type_name_hint)
            assert head == 'sequence'
            type_string = subtypes[0]

        serialization.encode(out, len(list))

        for item in list:
            if type_string == '':
                type_string = serialization.encode(
                    out, item, type_name_hint=type_string)
            else:
                assert type_string == serialization.encode(
                    out, item, type_name_hint=type_string
                ), "values with different types present in this sequence"

        return 'sequence<%s>' % (type_string)


class StringCodec(Codec):
    def decode(self, bytes, serialization):
        """decode a string

        :param bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        size = serialization.decode('uint64_t', bytes)

        return str(bytes.read(size), 'utf-8')

    def encode(self, out, val, serialization=None, *, type_name_hint=''):
        """encode a string to bytes

        :param _out: output list of byte arrays
        :param _val: string to encode
        :returns: "string"
        :rtype: string

        """
        if type_name_hint:
            assert type_name_hint == 'string'

        serialization.encode(out, len(val))
        out.write(val.encode())
        return 'string'


class OffsetCodec(Codec):
    def decode(self, bytes, serialization):
        """decode an Offset entry

        :param bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        elementId = serialization.decode('UUID', bytes)
        offset = serialization.decode('uint64_t', bytes)

        return Offset(elementId, displacement)

    def encode(self, out, val, serialization=None, *, type_name_hint=''):
        """
        encode Offset to bytes
        """
        if type_name_hint:
            assert type_name_hint == 'Offset'

        serialization.encode(out, val.element_id)
        serialization.encode(out, val.offset)
        return 'Offset'


class UUIDCodec(Codec):
    def decode(self, bytes, serialization):
        """decode a UUID entry

        :param bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        return uuid.UUID(bytes=bytes.read(16))

    def encode(self, out, val, serialization=None, *, type_name_hint=''):
        """encode UUID to bytes

        :param out: output list of byte arrays
        :param val: uuid to encode
        :returns: "UUID"
        :rtype: string

        """
        if type_name_hint:
            assert type_name_hint == 'UUID'

        out.write(val.bytes)
        return 'UUID'


class AddrCodec(Codec):
    def decode(self, bytes, serialization):
        """decode an Addr entry

        :param bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        addr = serialization.decode('uint64_t', bytes)
        return gtirb.Addr(addr)

    def encode(self, out, val, serialization=None, *, type_name_hint=''):
        """encode Addr to bytes

        :param out: output list of byte arrays
        :param val: Addr to encode
        :returns: 'Addr'
        :rtype: string

        """
        if type_name_hint:
            assert type_name_hint == 'Addr'

        serialization.encode(out, val.address)
        return 'Addr'


class Uint64Codec(Codec):
    def decode(self, bytes, serialization):
        """decode uint64_t

        :param bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        return int.from_bytes(bytes.read(8), byteorder='little', signed=False)

    def encode(self, out, val, serialization=None, *, type_name_hint=''):
        """encode uint64_t to bytes

        :param out: output list of byte arrays
        :param val: integer to encode
        :returns: 'uint64_t'
        :rtype: string

        """
        ret = 'uint64_t'
        if type_name_hint:
            assert type_name_hint == ret

        out.write(val.to_bytes(8, byteorder='little'))
        return ret


class Serialization:
    """
    A class used to encode/decode aux data table entries. Use the top
    level `register_codec`, `encode` and `decode` functions.
    """

    def getSubtypes(self, type_name):
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

    def getEncodedTypeMapping(self, type_name):
        return self.type_mapping.get(type_name, type_name)

    def __init__(self):
        """
        Initializes all encoders and decoders.
        """

        self.codecs = {
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
        self.type_mapping = {
            'dict': 'mapping',
            'int': 'uint64_t',
            'list': 'sequence',
            'set': 'set',
            'str': 'string'
        }

    def registerCodec(self, type_name, codec):
        """Register a Codec for a custom type. Use this method to
        register encode/decode functions for your own clpasses

        :param type_name: string type_name, this should be same as the
            __class__.__name__ field of the class
        :param codec: A Codec instance

        """
        assert type_name not in self.codecs, \
            'Type - %s already has a codec' % (type_name)

        self._codecs[type_name] = codec

    def encode(self, out, val, *, type_name_hint=''):
        """Top level encode function.

        :param _out: A list of byte arrays. The caller needs to
             flatten it to a byte stream if they so wish
        :param _val: Value to encode
        :returns: encoded string of the type name
        :rtype: string

        """
        assert isinstance(out, io.BytesIO)
        type_name = self.getEncodedTypeMapping(val.__class__.__name__)
        assert type_name in self.codecs, \
            'No encoder present for type_name - %s' % (type_name)

        return self.codecs[type_name].encode(out,
                                             val,
                                             self,
                                             type_name_hint=type_name_hint)

    def decode(self, type_name, bytes):
        """Top level decode function.

        :param type_name: top level type name
        :param _bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        assert isinstance(bytes, io.BytesIO)
        if '<' in type_name:
            head, subtypes = self.getSubtypes(type_name)
            assert head in self.codecs, \
                "No decoders present for type - %s." % (head)

            return self.codecs[head].decode(bytes, subtypes, self)
        else:
            assert type_name in self.codecs, \
                "No decoders present for type - %s." % (type_name)

            return self.codecs[type_name].decode(bytes, serialization=self)
