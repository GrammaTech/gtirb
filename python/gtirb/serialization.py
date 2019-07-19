"""
The GTIRB python serialization module.

Contains the `Serialization` class that is responsible for
encoding/decoding various GTIRB types to/from bytes and the `Codec`
class that holds the encode and decode functions for a type.

"""
from re import findall
from uuid import UUID

from gtirb.offset import Offset
from gtirb.node import Node


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


class TypeNameHintError(EncodeError):
    """Malformed type name hint"""
    def __init__(self, hint):
        super().__init__("malformed type name hint: '%s'" % (hint))


class Codec:
    """
    A class that holds the `encode` and `decode` methods for a type.
    """
    def __init__(self):
        pass

    def decode(self, raw_bytes, subtypes, serialization):
        pass

    def encode(self, out, item, serialization, *, type_name_hint=None):
        pass


class MappingCodec(Codec):
    def decode(self, raw_bytes, subtypes, serialization):
        """decode a mapping<..> entry

        :param bytes: Raw bytes (io.BytesIO typed)
        :param subtypes: A tuple containing the key/value types
        :param serialization: The Serializaton instance calling this
        :returns: decoded return type
        :rtype: tuple

        """
        try:
            key_type, val_type = subtypes
        except (TypeError, ValueError):
            raise DecodeError(
                "could not unpack mapping types: %s" % (subtypes))

        mapping = dict()
        mapping_len = serialization.decode('uint64_t', raw_bytes)
        for _ in range(mapping_len):
            key = serialization.decode_tree(key_type, raw_bytes)
            val = serialization.decode_tree(val_type, raw_bytes)
            mapping[key] = val

        return mapping

    def encode(self, out, mapping, serialization, *, type_name_hint=None):
        """encode a dict into bytes.

        :param out: output bytes in io.BytesIO type
        :param mapping: mapping to encode
        :param serialization: The Serialization instance calling this
        :returns: the encoded type_name string
        :rtype: string

        """
        if len(mapping) == 0 and type_name_hint is None:
            raise EncodeError("no type hint for encoding empty mapping")

        key_type = None
        val_type = None

        if type_name_hint is not None:
            parent, (key_type, val_type) = \
                Serialization.parse_type(type_name_hint)
            if parent != 'mapping':
                raise TypeNameHintError(type_name_hint)
            key_type = Serialization.type_tree_str(key_type)
            val_type = Serialization.type_tree_str(val_type)

        serialization.encode(out, len(mapping))

        for key, val in mapping.items():
            encoded_key_type = \
                serialization.encode(out, key, type_name_hint=key_type)
            if key_type is None:
                key_type = encoded_key_type
            elif key_type != encoded_key_type:
                raise EncodeError("keys with different types in mapping")

            encoded_val_type = \
                serialization.encode(out, val, type_name_hint=val_type)
            if val_type is None:
                val_type = encoded_val_type
            elif val_type != encoded_val_type:
                raise EncodeError("values with different types in mapping")

        return 'mapping<%s,%s>' % (key_type, val_type)


class SequenceCodec(Codec):
    """SequenceCodec and SetCodec have a "name" attribute because their
    encoders are almost exactly the same. This attribute is used to
    distinguish between the two."""
    name = 'sequence'

    def decode(self, raw_bytes, subtypes, serialization):
        """decode a sequence<..> entry

        :param raw_bytes: Raw bytes (io.BytesIO typed)
        :param subtypes: a singleton tuple containing a subtype
        :param serialization: The Serialization instance calling this
        :returns: decoded return type
        :rtype: tuple

        """
        try:
            sequence_type, = subtypes
        except (TypeError, ValueError) as e:
            raise DecodeError("could not unpack %s type: %s" % (self.name, e))

        sequence = list()
        sequence_len = serialization.decode('uint64_t', raw_bytes)
        for _ in range(sequence_len):
            sequence.append(
                serialization.decode_tree(sequence_type, raw_bytes)
            )

        return sequence

    def encode(self, out, sequence, serialization, *, type_name_hint=None):
        """encode a list to bytes

        :param out: list of byte arrays
        :param list: list to encode
        :param serialization: The Serialization instance calling this
        :returns: encoded type name string
        :rtype: string

        """

        if len(sequence) == 0 and type_name_hint is None:
            raise EncodeError("no type hint for empty %s" % (self.name))

        if type_name_hint is not None:
            head, (subtype,) = Serialization.parse_type(type_name_hint)
            if head != self.name:
                raise TypeNameHintError(type_name_hint)
            subtype = Serialization.type_tree_str(subtype)

        serialization.encode(out, len(sequence))

        for item in sequence:
            encoded_type = \
                serialization.encode(out, item, type_name_hint=subtype)
            if subtype is None:
                subtype = encoded_type
            elif subtype != encoded_type:
                raise EncodeError(
                    "values with different types in %s" % (self.name)
                )

        return '%s<%s>' % (self.name, subtype)


class SetCodec(SequenceCodec):
    """SetCodec reads in a sequence and converts it to a set"""
    name = 'set'

    def decode(self, raw_bytes, subtypes, serialization):
        return set(super().decode(raw_bytes, subtypes, serialization))


class StringCodec(Codec):
    def decode(self, raw_bytes, subtypes, serialization):
        """decode a string

        :param raw_bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        size = serialization.decode('uint64_t', raw_bytes)

        return str(raw_bytes.read(size), 'utf-8')

    def encode(self, out, val, serialization=None, *, type_name_hint=None):
        """encode a string to bytes

        :param _out: output list of byte arrays
        :param _val: string to encode
        :returns: "string"
        :rtype: string

        """
        if type_name_hint is not None and type_name_hint != 'string':
            raise EncodeError("wrong type hint for string encoding")

        serialization.encode(out, len(val))
        out.write(val.encode())
        return 'string'


class OffsetCodec(Codec):
    def decode(self, raw_bytes, subtypes, serialization):
        """decode an Offset entry

        :param raw_bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        element_uuid = serialization.decode('UUID', raw_bytes)
        displacement = serialization.decode('uint64_t', raw_bytes)

        return Offset(element_uuid, displacement)

    def encode(self, out, val, serialization=None, *, type_name_hint=None):
        """
        encode Offset to bytes
        """
        if type_name_hint is not None and type_name_hint != 'Offset':
            raise EncodeError("wrong type hint for offset")

        serialization.encode(out, val.element_id)
        serialization.encode(out, val.displacement)
        return 'Offset'


class UUIDCodec(Codec):
    def decode(self, raw_bytes, subtypes, serialization):
        """decode a UUID entry

        :param raw_bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        return UUID(bytes=raw_bytes.read(16))

    def encode(self, out, val, serialization=None, *, type_name_hint=None):
        """encode UUID to bytes

        :param out: output list of byte arrays
        :param val: uuid to encode
        :returns: "UUID"
        :rtype: string

        """
        if type_name_hint is not None and type_name_hint != 'UUID':
            raise EncodeError("wrong type hint for UUID")

        out.write(val.bytes)
        return 'UUID'


class AddrCodec(Codec):
    class Addr:
        """A class for holding addresses. This is defined because codecs are
        selected based on the class name during encoding."""
        def __init__(self, address=None):
            self.address = address

        def __eq__(self, other):
            return isinstance(other, type(self)) and \
                self.address == other.address

        def __hash__(self):
            return hash(self.address)

    def decode(self, raw_bytes, subtypes, serialization):
        """decode an Addr entry

        :param raw_bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        addr = serialization.decode('uint64_t', raw_bytes)
        return AddrCodec.Addr(addr)

    def encode(self, out, val, serialization=None, *, type_name_hint=None):
        """encode Addr to bytes

        :param out: output list of byte arrays
        :param val: Addr to encode
        :returns: 'Addr'
        :rtype: string

        """
        if type_name_hint is not None and type_name_hint != 'Addr':
            raise EncodeError("wrong type hint for Addr")

        serialization.encode(out, val.address)
        return 'Addr'


class Uint64Codec(Codec):
    def decode(self, raw_bytes, subtypes, serialization):
        """decode uint64_t

        :param raw_bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        return int.from_bytes(raw_bytes.read(8),
                              byteorder='little', signed=False)

    def encode(self, out, val, serialization=None, *, type_name_hint=None):
        """encode uint64_t to bytes

        :param out: output list of byte arrays
        :param val: integer to encode
        :returns: 'uint64_t'
        :rtype: string

        """
        if type_name_hint is not None and type_name_hint != 'uint64_t':
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

    @staticmethod
    def parse_type(type_name):
        """ Given an encoded aux_data type_name, generate its parse tree
        A single parsed type is a tuple of the type name and a tuple of its
        subtypes, an empty tuple indicates no subtype.

        Examples:
          parse_type('foo') == ('foo', ())
          parse_type('foo<bar>') ==  ('foo', (('bar',()),))
          parse_type('foo<bar<baz>>') == ('foo', (('bar', (('baz', ()),)),))

        :param type_name: encoded type name.
        :returns: a list of parsed type/subtype tuples
        :rtype: list

        """

        tokens = findall('[^<>,]+|<|>|,', type_name)

        def parse(tokens, tree):
            tree = list(tree)
            # It is an error to parse nothing
            if len(tokens) == 0:
                raise TypeNameHintError(type_name)
            first_token, *tail = tokens

            # The first token should be a name
            if first_token in {'<', '>', ','}:
                raise TypeNameHintError(type_name)

            # Base case
            if len(tail) == 0:
                tree.append((first_token, ()))
                return tuple(tree), []
            next_token, *tail = tail

            # No subtypes
            if next_token == ',':
                tree.append((first_token, ()))

            # Parse subtypes
            if next_token == '<':
                # Extract just the subtype tokens and parse them
                stack = ['<']
                subtype_tokens = list()
                remaining_tokens = list()
                for t in tail:
                    if len(stack) == 0:
                        remaining_tokens.append(t)
                        continue
                    if t == '<':
                        stack.append(t)
                    elif t == '>':
                        stack.pop()
                    subtype_tokens.append(t)
                if len(stack) > 0 or subtype_tokens[-1] != '>':
                    raise TypeNameHintError(type_name)
                subtypes, remaining = parse(subtype_tokens[:-1], [])
                # Parsing should consume all subtype tokens
                if len(remaining) != 0:
                    raise TypeNameHintError(type_name)
                tree.append((first_token, subtypes))
                # Finish if all tokens are consumed
                if len(remaining_tokens) == 0:
                    return tuple(tree), []
                next_token, *tail = remaining_tokens

            # If the next token is a comma, parse next
            if next_token == ',':
                return parse(tail, tree)

            # None of the rules match, error
            raise TypeNameHintError(type_name)
        # There should only be one item at the root of the tree
        try:
            parse_tree, = parse(tokens, [])[0]
        except ValueError:
            raise TypeNameHintError(type_name)
        return parse_tree

    @staticmethod
    def type_tree_str(type_tree):
        """Returns a string corresponding to a type tree in the same format
        created by Serialization.parse_type()
        """
        type_name, subtypes = type_tree
        if len(subtypes) == 0:
            return type_name
        subtype_names = \
            ','.join(Serialization.type_tree_str(subt) for subt in subtypes)
        return '%s<%s>' % (type_name, subtype_names)

    def register_codec(self, type_name, codec):
        """Register a Codec for a custom type. Use this method to
        register encode/decode functions for your own classes

        :param type_name: string type_name, this should be same as the
            __class__.__name__ field of the class
        :param codec: A Codec instance

        """
        if type_name in self.codecs:
            raise KeyError("codec already registered for %s" % (type_name))
        self.codecs[type_name] = codec

    def encode(self, out, val, *, type_name_hint=None):
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

    def decode_tree(self, type_tree, raw_bytes):
        """Decodes given a parsed type tree

        :param type_tree: parsed type tree
        :param raw_bytes: io.BytesIO
        :returns: decoded return type

        """
        try:
            type_name, subtypes = type_tree
        except ValueError:
            raise DecodeError(
                "could not unpack type tree %s" % (str(type_tree)))
        if type_name not in self.codecs:
            raise DecodeError("no decoder for %s" % (type_name))
        return self.codecs[type_name].decode(raw_bytes, subtypes, self)

    def decode(self, type_name, raw_bytes):
        """Top level decode function.

        :param type_name: top level type name
        :param raw_bytes: Raw bytes (io.BytesIO typed)
        :returns: decoded return type
        :rtype: tuple

        """
        parse_tree = Serialization.parse_type(type_name)
        return self.decode_tree(parse_tree, raw_bytes)
