# -*- coding: utf-8 -*-
"""
The GTIRB python serialization module.

Contains the `Serialization` class that is responsible for
encoding/decoding various GTIRB types to/from bytes and the `Codec`
class that holds the encode and decode functions for a type.

"""
import gtirb
from io import BytesIO
from uuid import UUID


class CodecError(Exception):
    """Base class for codec exceptions"""
    pass


class DecodeError(CodecError):
    """Exception during decoding"""
    def __init__(self, msg):
        self.msg = msg


class EncodeError(CodecError):
    """Exception during encoding"""
    def __init__(self, msg):
        self.msg = msg


class TypeHintError(EncodeError):
    """Malformed type hint"""
    def __init__(self):
        self.args = ("malformed type hint",)


class Codec:
    """
    A class that holds the `encode` and `decode` methods for a type.
    """
    def __init__(self):
        pass

    def decode(self, raw_bytes, sub_types, serialization):
        pass

    def encode(self, out, item, serialization, *, type_name_hint=''):
        pass


class MappingCodec(Codec):
    def decode(self, raw_bytes, sub_types, serialization):
        """decode a mapping<..> entry

        :param bytes: Raw bytes (io.BytesIO typed)
        :param sub_types: List of subtype names
        :param serialization: The Serializaton instance calling this
        :returns: decoded return type
        :rtype: tuple

        """
        try:
            key_type, val_type = sub_types
        except Exception:
            raise DecodeError("could not unpack mapping types")

        mapping = dict()
        mapping_len = serialization.decode('uint64_t', raw_bytes)
        for _ in range(mapping_len):
            key = serialization.decode(key_type, raw_bytes)
            val = serialization.decode(val_type, raw_bytes)
            mapping[key] = val

        return mapping

    def encode(self, out, mapping, serialization, *, type_name_hint=''):
        """encode a dict into bytes.

        :param out: output bytes in io.BytesIO type
        :param mapping: mapping to encode
        :param serialization: The Serialization instance calling this
        :returns: the encoded type_name string
        :rtype: string

        """
        if len(mapping) == 0 and not type_name_hint:
            raise EncodeError("no type hint for encoding empty mapping")

        key_type = ''
        val_type = ''

        if type_name_hint:
            if '<' not in type_name_hint:
                raise TypeHintError
            head, (key_type, val_type) = \
                serialization.get_subtypes(type_name_hint)
            if head != 'mapping':
                raise TypeHintError

        serialization.encode(out, len(mapping))

        for key, val in mapping.items():
            encoded_key_type = \
                serialization.encode(out, key, type_name_hint=key_type)
            if key_type == '':
                key_type = encoded_key_type
            elif key_type != encoded_key_type:
                raise EncodeError("keys with different types in mapping")

            encoded_val_type = \
                serialization.encode(out, val, type_name_hint=val_type)
            if val_type == '':
                val_type = encoded_val_type
            elif val_type != encoded_val_type:
                raise EncodeError("values with different types in mapping")

        return 'mapping<%s,%s>' % (key_type, val_type)


class SequenceCodec(Codec):
    name = 'sequence'

    def decode(self, raw_bytes, sub_types, serialization):
        """decode a sequence<..> entry

        :param raw_bytes: Raw bytes (io.BytesIO typed)
        :param sub_types: a list of sub-type names
        :param serialization: The Serialization instance calling this
        :returns: decoded return type
        :rtype: tuple

        """
        try:
            subtype, = sub_types
        except Exception:
            raise DecodeError("could not unpack %s type" % (self.name))

        sequence = list()
        sequence_len = serialization.decode('uint64_t', raw_bytes)
        for _ in range(sequence_len):
            sequence.append(serialization.decode(subtype, raw_bytes))

        return sequence

    def encode(self, out, sequence, serialization, *, type_name_hint=''):
        """encode a list to bytes

        :param out: list of byte arrays
        :param list: list to encode
        :param serialization: The Serialization instance calling this
        :returns: encoded type name string
        :rtype: string

        """
        subtype = ''

        if len(sequence) == 0 and not type_name_hint:
            raise EncodeError("no type hint for empty %s" % (self.name))

        if type_name_hint:
            if '<' not in type_name_hint:
                raise TypeHintError
            head, subtypes = serialization.get_subtypes(type_name_hint)
            if head != self.name:
                raise TypeHintError
            subtype = subtypes[0]

        serialization.encode(out, len(sequence))

        for item in sequence:
            encoded_type = \
                serialization.encode(out, item, type_name_hint=subtype)
            if subtype == '':
                subtype = encoded_type
            elif subtype != encoded_type:
                raise EncodeError(
                    "values with different types in %s" % (self.name)
                )

        return '%s<%s>' % (self.name, subtype)


class SetCodec(SequenceCodec):
    """SetCodec reads in a sequence and converts it to a set"""
    name = 'set'

    def decode(self, raw_bytes, sub_types, serialization):
        return set(super().decode(raw_bytes, sub_types, serialization))


class StringCodec(Codec):
    def decode(self, raw_bytes, serialization):
        """decode a string

        :param raw_bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        size = serialization.decode('uint64_t', raw_bytes)

        return str(raw_bytes.read(size), 'utf-8')

    def encode(self, out, val, serialization=None, *, type_name_hint=''):
        """encode a string to bytes

        :param _out: output list of byte arrays
        :param _val: string to encode
        :returns: "string"
        :rtype: string

        """
        if type_name_hint and type_name_hint != string:
            raise EncodeError("wrong type hint for string encoding")

        serialization.encode(out, len(val))
        out.write(val.encode())
        return 'string'


class OffsetCodec(Codec):
    def decode(self, raw_bytes, serialization):
        """decode an Offset entry

        :param raw_bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        element_uuid = serialization.decode('UUID', raw_bytes)
        offset = serialization.decode('uint64_t', raw_bytes)

        return Offset(element_uuid, offset)

    def encode(self, out, val, serialization=None, *, type_name_hint=''):
        """
        encode Offset to bytes
        """
        if type_name_hint and type_name_hint != 'Offset':
            raise EncodeError("wrong type hint for offset")

        serialization.encode(out, val.element_id)
        serialization.encode(out, val.offset)
        return 'Offset'


class UUIDCodec(Codec):
    def decode(self, raw_bytes, serialization):
        """decode a UUID entry

        :param raw_bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        return UUID(bytes=raw_bytes.read(16))

    def encode(self, out, val, serialization=None, *, type_name_hint=''):
        """encode UUID to bytes

        :param out: output list of byte arrays
        :param val: uuid to encode
        :returns: "UUID"
        :rtype: string

        """
        if type_name_hint and type_name_hint != 'UUID':
            raise EncodeError("wrong type hint for UUID")

        out.write(val.bytes)
        return 'UUID'


class AddrCodec(Codec):
    def decode(self, raw_bytes, serialization):
        """decode an Addr entry

        :param raw_bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        addr = serialization.decode('uint64_t', raw_bytes)
        return gtirb.Addr(addr)

    def encode(self, out, val, serialization=None, *, type_name_hint=''):
        """encode Addr to bytes

        :param out: output list of byte arrays
        :param val: Addr to encode
        :returns: 'Addr'
        :rtype: string

        """
        if type_name_hint and type_name_hint != 'Addr':
            raise EncodeError("wrong type hint for Addr")

        serialization.encode(out, val.address)
        return 'Addr'


class Uint64Codec(Codec):
    def decode(self, raw_bytes, serialization):
        """decode uint64_t

        :param raw_bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        return int.from_bytes(raw_bytes.read(8),
                              byteorder='little', signed=False)

    def encode(self, out, val, serialization=None, *, type_name_hint=''):
        """encode uint64_t to bytes

        :param out: output list of byte arrays
        :param val: integer to encode
        :returns: 'uint64_t'
        :rtype: string

        """
        if type_name_hint and type_name_hint != 'uint64_t':
            raise EncodeError("wrong type hint for uint64_t")

        out.write(val.to_bytes(8, byteorder='little'))
        return 'uint64_t'


class Serialization:
    """
    A class used to encode/decode aux data table entries. Use the top
    level `register_codec`, `encode` and `decode` functions.
    """
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

    def register_codec(self, type_name, codec):
        """Register a Codec for a custom type. Use this method to
        register encode/decode functions for your own clpasses

        :param type_name: string type_name, this should be same as the
            __class__.__name__ field of the class
        :param codec: A Codec instance

        """
        if type_name in self.codecs:
            raise KeyError("codec already registered for %s" % (type_name))
        self.codecs[type_name] = codec

    def encode(self, out, val, *, type_name_hint=''):
        """Top level encode function.

        :param _out: A list of byte arrays. The caller needs to
             flatten it to a byte stream if they so wish
        :param _val: Value to encode
        :returns: encoded string of the type name
        :rtype: string

        """
        class_name = val.__class__.__name__
        type_name = self.type_mapping.get(class_name, class_name)

        if type_name not in self.codecs:
            raise EncodeError("no encoder for %s" % (type_name))

        codec = self.codecs[type_name]
        return codec.encode(out, val, self, type_name_hint=type_name_hint)

    def decode(self, type_name, raw_bytes):
        """Top level decode function.

        :param type_name: top level type name
        :param raw_bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        if not isinstance(raw_bytes, BytesIO):
            raise ValueError("raw_bytes is not a BytesIO")
        if '<' in type_name:
            head, subtypes = self.get_subtypes(type_name)
            if head not in self.codecs:
                raise DecodeError("no decoder for %s" % (head))
            return self.codecs[head].decode(raw_bytes, subtypes, self)
        else:
            if type_name not in self.codecs:
                raise DecodeError("no decoder for %s" % (type_name))
            return self.codecs[type_name].decode(raw_bytes, serialization=self)
