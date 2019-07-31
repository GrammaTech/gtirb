"""Python serialization module.

The Serialization class is used for encoding/decoding GTIRB types using
codecs definded from the Codec base class

"""
from re import findall
from uuid import UUID

from .addr import Addr
from .offset import Offset
from .node import Node


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


class TypeNameError(EncodeError):
    """Malformed type name"""
    def __init__(self, hint):
        super().__init__("malformed type name: '%s'" % (hint))


class Codec:
    """Base class for codecs."""

    @staticmethod
    def decode(raw_bytes, *, serialization=None, subtypes=tuple()):
        """Decodes data with possible subtypes encoded in raw_bytes.
        Should return an new decoded object.

        Parameters:
            raw_bytes: the BytesIO object to be decoded
            serialization: optional Serialization instance used to invoke
                custom codecs that might be needed by containers
            subtypes: optional parsed tree of subtypes that might be needed
                by containers

        Returns:
            a new Python object decoded from raw_bytes

        """
        raise NotImplementedError

    @staticmethod
    def encode(out, item, *, serialization=None, subtypes=tuple()):
        """Encodes an item, writing the serialized object to out,
        a BytesIO instance. Optionally takes a type name hint.

        Parameters:
            out: the BytesIO channel to serialize to
            item: the Python object to encode
            serialization: optional Serialization instance used to invoke
                custom codecs that might be needed by containers
            subtypes: optional parsed tree of subtypes that might be needed
                by containers

        Returns:
            string corresponding to the encoded type

        """
        raise NotImplementedError


class AddrCodec(Codec):
    """Codec for Addr"""

    @staticmethod
    def decode(raw_bytes, *, serialization=None, subtypes=tuple()):
        if subtypes != ():
            raise DecodeError("Addr should have no subtypes")
        addr = Uint64Codec.decode(raw_bytes)
        return Addr(addr)

    @staticmethod
    def encode(out, val, *, serialization=None, subtypes=tuple()):
        if subtypes != ():
            raise EncodeError("Addr should have no subtypes")
        Uint64Codec.encode(out, val.address)
        return 'Addr'


class MappingCodec(Codec):
    """Codec for mapping<..> entries. Used for encoding Python dicts"""

    @staticmethod
    def decode(raw_bytes, *, serialization, subtypes):
        try:
            key_type, val_type = subtypes
        except (TypeError, ValueError):
            raise DecodeError(
                "could not unpack mapping types: %s" % str(subtypes))
        mapping = dict()
        mapping_len = Uint64Codec.decode(raw_bytes)
        for _ in range(mapping_len):
            key = serialization._decode_tree(raw_bytes, key_type)
            val = serialization._decode_tree(raw_bytes, val_type)
            mapping[key] = val
        return mapping

    @staticmethod
    def encode(out, mapping, *, serialization, subtypes):
        try:
            key_type, val_type = subtypes
        except (TypeError, ValueError):
            raise EncodeError(
                "could not unpack mapping types: %s" % str(subtypes))
        Uint64Codec.encode(out, len(mapping))

        for key, val in mapping.items():
            serialization._encode_tree(out, key, key_type)
            serialization._encode_tree(out, val, val_type)
        key_type_name = Serialization._type_tree_str(key_type)
        val_type_name = Serialization._type_tree_str(val_type)
        return 'mapping<%s,%s>' % (key_type_name, val_type_name)


class OffsetCodec(Codec):
    """Codec for Offsets, containing a UUID and a uint64_t displacement"""

    @staticmethod
    def decode(raw_bytes, *, serialization=None, subtypes=tuple()):
        if subtypes != ():
            raise DecodeError("Offset should have no subtypes")
        element_uuid = UUIDCodec.decode(raw_bytes)
        displacement = Uint64Codec.decode(raw_bytes)

        return Offset(element_uuid, displacement)

    @staticmethod
    def encode(out, val, *, serialization=None, subtypes=tuple()):
        if subtypes != ():
            raise EncodeError("Offset should have no subtypes")
        UUIDCodec.encode(out, val.element_id)
        Uint64Codec.encode(out, val.displacement)
        return 'Offset'


class SequenceCodec(Codec):
    """Codec for sequence<..> entries. Encodes Python lists/tuples."""

    @staticmethod
    def decode(raw_bytes, *, serialization, subtypes):
        try:
            subtype, = subtypes
        except (TypeError, ValueError) as e:
            raise DecodeError("could not unpack sequence type: %s" % str(e))
        sequence = list()
        sequence_len = Uint64Codec.decode(raw_bytes)
        for _ in range(sequence_len):
            sequence.append(serialization._decode_tree(raw_bytes, subtype))
        return sequence

    @staticmethod
    def encode(out, sequence, *, serialization, subtypes):
        try:
            subtype, = subtypes
        except (TypeError, ValueError) as e:
            raise EncodeError("could not unpack sequence type: %s" % str(e))
        subtype_name = Serialization._type_tree_str(subtype)
        Uint64Codec.encode(out, len(sequence))
        for item in sequence:
            serialization._encode_tree(out, item, subtype)
        return 'sequence<%s>' % subtype_name


class SetCodec(Codec):
    """Codec for set<..> entries. Used for encoding Python sets."""

    @staticmethod
    def decode(raw_bytes, *, serialization, subtypes):
        try:
            subtype, = subtypes
        except (TypeError, ValueError) as e:
            raise DecodeError("could not unpack set type: %s" % str(e))
        decoded_set = set()
        set_len = Uint64Codec.decode(raw_bytes)
        for _ in range(set_len):
            decoded_set.add(serialization._decode_tree(raw_bytes, subtype))
        return decoded_set

    @staticmethod
    def encode(out, items, *, serialization, subtypes):
        try:
            subtype, = subtypes
        except (TypeError, ValueError) as e:
            raise EncodeError("could not unpack set type: %s" % str(e))
        subtype_name = Serialization._type_tree_str(subtype)
        Uint64Codec.encode(out, len(items))
        for item in items:
            serialization._encode_tree(out, item, subtype)
        return 'set<%s>' % subtype_name


class StringCodec(Codec):
    """Codec for strings"""

    @staticmethod
    def decode(raw_bytes, *, serialization=None, subtypes=tuple()):
        if subtypes != tuple():
            raise DecodeError("string should have no subtypes")
        size = Uint64Codec.decode(raw_bytes)
        return str(raw_bytes.read(size), 'utf-8')

    @staticmethod
    def encode(out, val, *, serialization=None, subtypes=tuple()):
        if subtypes != ():
            raise EncodeError("string should have no subtypes")
        Uint64Codec.encode(out, len(val))
        out.write(val.encode())
        return 'string'


class Uint64Codec(Codec):
    """Codec for uint64_t"""

    @staticmethod
    def decode(raw_bytes, *, serialization=None, subtypes=tuple()):
        if subtypes != ():
            raise DecodeError("uint64_t should have no subtypes")

        return int.from_bytes(raw_bytes.read(8),
                              byteorder='little',
                              signed=False)

    @staticmethod
    def encode(out, val, *, serialization=None, subtypes=tuple()):
        if subtypes != ():
            raise EncodeError("uint64_t should have no subtypes")

        out.write(val.to_bytes(8, byteorder='little'))
        return 'uint64_t'


class UUIDCodec(Codec):
    """Codec for raw UUIDs or Nodes

    Decoding a UUID first checks the Node cache for an object with the
    corresponding UUID, and either returns the object it hits or a new
    raw UUID.

    """
    @staticmethod
    def decode(raw_bytes, *, serialization=None, subtypes=tuple()):
        if subtypes != ():
            raise DecodeError("UUID should have no subtypes")
        uuid = UUID(bytes=raw_bytes.read(16))
        if uuid in Node._uuid_cache:
            return Node._uuid_cache[uuid]
        return uuid

    @staticmethod
    def encode(out, val, *, serialization=None, subtypes=tuple()):
        if subtypes != ():
            raise EncodeError("UUID should have no subtypes")
        if isinstance(val, Node):
            out.write(val.uuid.bytes)
        elif isinstance(val, UUID):
            out.write(val.bytes)
        else:
            raise EncodeError("UUID codec only supports UUIDs or Nodes")
        return 'UUID'


class Serialization:
    """Tracks codecs used to serialize/deserialize GTIRB objects

    Attributes:
        codecs: dictionary mapping type names to codecs. Codecs can be added
            or overridden using this dictionary.
        type_mapping: a dictionary mapping Python types to encoded GTIRB types

    """
    def __init__(self):
        self.codecs = {
            'Addr': AddrCodec,
            'Offset': OffsetCodec,
            'mapping': MappingCodec,
            'sequence': SequenceCodec,
            'set': SetCodec,
            'string': StringCodec,
            'uint64_t': Uint64Codec,
            'UUID': UUIDCodec,
        }
        self.type_mapping = {
            'dict': 'mapping',
            'int': 'uint64_t',
            'list': 'sequence',
            'set': 'set',
            'str': 'string',
        }

    def _decode_tree(self, raw_bytes, type_tree):
        """Decodes given a parsed type tree"""
        try:
            type_name, subtypes = type_tree
        except ValueError:
            raise DecodeError(
                "could not unpack type tree %s" % str(type_tree))
        if type_name not in self.codecs:
            raise DecodeError("no decoder for %s" % type_name)
        codec = self.codecs[type_name]
        return codec.decode(raw_bytes, serialization=self, subtypes=subtypes)

    def _encode_tree(self, out, val, type_tree):
        try:
            type_name, subtypes = type_tree
        except ValueError:
            raise EncodeError(
                "could not unpack type tree %s" % str(type_tree))
        if type_name not in self.codecs:
            raise EncodeError("no encoder for %s" % type_name)
        codec = self.codecs[type_name]
        return codec.encode(out, val, serialization=self, subtypes=subtypes)

    @staticmethod
    def _parse_type(type_name):
        """ Given an encoded aux_data type_name, generate its parse tree
        A single parsed type is a tuple of the type name and a tuple of its
        subtypes, an empty tuple indicates no subtype.

        Examples:
          _parse_type('foo') == ('foo', ())
          _parse_type('foo<bar>') ==  ('foo', (('bar',()),))
          _parse_type('foo<bar<baz>>') == ('foo', (('bar', (('baz', ()),)),))

        Returns:
           a nested tuple of parsed type/subtype tuples

        """
        tokens = findall('[^<>,]+|<|>|,', type_name)

        def parse(tokens, tree):
            tree = list(tree)
            # It is an error to parse nothing
            if len(tokens) == 0:
                raise TypeNameError(type_name)
            first_token, *tail = tokens

            # The first token should be a name
            if first_token in {'<', '>', ','}:
                raise TypeNameError(type_name)

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
                    raise TypeNameError(type_name)
                subtypes, remaining = parse(subtype_tokens[:-1], [])
                # Parsing should consume all subtype tokens
                if len(remaining) != 0:
                    raise TypeNameError(type_name)
                tree.append((first_token, subtypes))
                # Finish if all tokens are consumed
                if len(remaining_tokens) == 0:
                    return tuple(tree), []
                next_token, *tail = remaining_tokens

            # If the next token is a comma, parse next
            if next_token == ',':
                return parse(tail, tree)

            # None of the rules match, error
            raise TypeNameError(type_name)
        # There should only be one item at the root of the tree
        try:
            parse_tree, = parse(tokens, [])[0]
        except ValueError:
            raise TypeNameError(type_name)
        return parse_tree

    @staticmethod
    def _type_tree_str(type_tree):
        """Returns a string corresponding to a type tree in the same format
        created by Serialization._parse_type()

        """
        type_name, subtypes = type_tree
        if len(subtypes) == 0:
            return type_name
        subtype_names = \
            ','.join(Serialization._type_tree_str(subt) for subt in subtypes)
        return '%s<%s>' % (type_name, subtype_names)

    def decode(self, raw_bytes, type_name):
        """Top level decode function."""
        parse_tree = Serialization._parse_type(type_name)
        return self._decode_tree(raw_bytes, parse_tree)

    def encode(self, out, val, type_name):
        """Top level encode function."""
        parse_tree = Serialization._parse_type(type_name)
        return self._encode_tree(out, val, parse_tree)
