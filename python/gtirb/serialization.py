import io
import typing
from re import findall
from uuid import UUID

from .node import Node
from .offset import Offset

if typing.TYPE_CHECKING:
    CacheLookupFn = typing.Optional[
        typing.Callable[[UUID], typing.Optional[Node]]
    ]


class CodecError(Exception):
    """Base class for codec exceptions."""


class DecodeError(CodecError):
    """An exception during decoding."""


class EncodeError(CodecError):
    """An exception during encoding."""


class TypeNameError(EncodeError):
    """A type name is malformed."""

    def __init__(self, hint):
        # type: (str) -> None
        super().__init__("malformed type name: '%s'" % (hint))


class UnknownCodecError(CodecError):
    """An unknown codec name is encountered.
    Caught and handled by the top-level codec methods.

    :param name: the name of the unknown codec
    """

    def __init__(self, name):
        # type: (str) -> None
        self.name = name


SubtypeTree = typing.Tuple[str, typing.Iterable["SubtypeTree"]]
"""A type hint representing a parsed serialization type name.
A ``SubtypeTree`` is a ``tuple`` with two items: A ``str`` giving
the name of the type and a ``tuple`` of type parameters
(which are also ``SubtypeTree``\\s). For example, the following are all
valid ``SubtypeTree``\\s:

>>> ('string', ())
>>> ('sequence', (('UUID',()),))
>>> ('mapping', (('string', ()),('set', (('UUID', ()),))))
"""


class Codec:
    """The base class for codecs."""

    @staticmethod
    def decode(
        raw_bytes,  # type: typing.BinaryIO
        *,
        serialization=None,  # type: Serialization
        subtypes=tuple(),  # type: SubtypeTree
        get_by_uuid=None  # type: CacheLookupFn
    ):
        # type: (...) -> typing.Any
        """Decode the specified raw data into a Python object.

        :param raw_bytes: The BytesIO object to be decoded.
        :param serialization: A Serialization instance used to invoke
            other codecs if needed.
        :param subtypes: The parsed type of this object.
        :param get_by_uuid: A function to look up nodes by UUID.
        :returns: A new Python object, as decoded from ``raw_bytes``.
        """

        raise NotImplementedError

    @staticmethod
    def encode(
        out,  # type: typing.BinaryIO
        item,  # type: typing.Any
        *,
        serialization=None,  # type: Serialization
        subtypes=tuple()  # type: SubtypeTree
    ):
        # type: (...) -> None
        """Encode an item, writing the serialized object to ``out``.

        :param out: A binary stream to serialize to.
        :param item: The arbitrary Python object to encode.
        :param serialization: A Serialization instance, used to invoke
            other codecs if needed.
        :param subtypes: The parsed type of this object.
        """

        raise NotImplementedError


class Int64Codec(Codec):
    """A Codec for 64-bit signed integers."""

    @staticmethod
    def decode(
        raw_bytes, *, serialization=None, subtypes=tuple(), get_by_uuid=None
    ):
        if subtypes != ():
            raise DecodeError("int64_t should have no subtypes")
        return int.from_bytes(
            raw_bytes.read(8), byteorder="little", signed=True
        )

    @staticmethod
    def encode(out, val, *, serialization=None, subtypes=tuple()):
        if subtypes != ():
            raise EncodeError("int64_t should have no subtypes")
        out.write(val.to_bytes(8, byteorder="little", signed=True))


class MappingCodec(Codec):
    """A Codec for mapping<K,V> entries. Implemented via ``dict``."""

    @staticmethod
    def decode(raw_bytes, *, serialization, subtypes, get_by_uuid=None):
        try:
            key_type, val_type = subtypes
        except (TypeError, ValueError):
            raise DecodeError(
                "could not unpack mapping types: %s" % str(subtypes)
            )
        mapping = dict()
        mapping_len = Uint64Codec.decode(raw_bytes)
        for _ in range(mapping_len):
            key = serialization._decode_tree(raw_bytes, key_type, get_by_uuid)
            val = serialization._decode_tree(raw_bytes, val_type, get_by_uuid)
            mapping[key] = val
        return mapping

    @staticmethod
    def encode(out, mapping, *, serialization, subtypes):
        try:
            key_type, val_type = subtypes
        except (TypeError, ValueError):
            raise EncodeError(
                "could not unpack mapping types: %s" % str(subtypes)
            )
        Uint64Codec.encode(out, len(mapping))
        for key, val in mapping.items():
            serialization._encode_tree(out, key, key_type)
            serialization._encode_tree(out, val, val_type)


class OffsetCodec(Codec):
    """A Codec for :class:`gtirb.Offset` objects,
    containing a UUID and a displacement.
    """

    @staticmethod
    def decode(
        raw_bytes, *, serialization=None, subtypes=tuple(), get_by_uuid=None
    ):
        if subtypes != ():
            raise DecodeError("Offset should have no subtypes")
        element_uuid = UUIDCodec.decode(raw_bytes, get_by_uuid=get_by_uuid)
        displacement = Uint64Codec.decode(raw_bytes)

        return Offset(element_uuid, displacement)

    @staticmethod
    def encode(out, val, *, serialization=None, subtypes=tuple()):
        if subtypes != ():
            raise EncodeError("Offset should have no subtypes")
        UUIDCodec.encode(out, val.element_id)
        Uint64Codec.encode(out, val.displacement)


class SequenceCodec(Codec):
    """A Codec for sequence<T> entries. Implemented via ``list``."""

    @staticmethod
    def decode(raw_bytes, *, serialization, subtypes, get_by_uuid=None):
        try:
            (subtype,) = subtypes
        except (TypeError, ValueError) as e:
            raise DecodeError("could not unpack sequence type: %s" % str(e))
        sequence = list()
        sequence_len = Uint64Codec.decode(raw_bytes)
        for _ in range(sequence_len):
            sequence.append(
                serialization._decode_tree(raw_bytes, subtype, get_by_uuid)
            )
        return sequence

    @staticmethod
    def encode(out, sequence, *, serialization, subtypes):
        try:
            (subtype,) = subtypes
        except (TypeError, ValueError) as e:
            raise EncodeError("could not unpack sequence type: %s" % str(e))
        Uint64Codec.encode(out, len(sequence))
        for item in sequence:
            serialization._encode_tree(out, item, subtype)


class SetCodec(Codec):
    """A Codec for set<T> entries. Implemented via ``set``."""

    @staticmethod
    def decode(raw_bytes, *, serialization, subtypes, get_by_uuid=None):
        try:
            (subtype,) = subtypes
        except (TypeError, ValueError) as e:
            raise DecodeError("could not unpack set type: %s" % str(e))
        decoded_set = set()
        set_len = Uint64Codec.decode(raw_bytes)
        for _ in range(set_len):
            decoded_set.add(
                serialization._decode_tree(raw_bytes, subtype, get_by_uuid)
            )
        return decoded_set

    @staticmethod
    def encode(out, items, *, serialization, subtypes):
        try:
            (subtype,) = subtypes
        except (TypeError, ValueError) as e:
            raise EncodeError("could not unpack set type: %s" % str(e))
        Uint64Codec.encode(out, len(items))
        for item in items:
            serialization._encode_tree(out, item, subtype)


class TupleCodec(Codec):
    """A Codec for tuple<...> entries. Implemented via ``tuple``."""

    @staticmethod
    def decode(raw_bytes, *, serialization, subtypes, get_by_uuid=None):
        # The length of a tuple is not contained in the Protobuf
        # representation, so error checking cannot be done here.
        decoded_list = list()
        for subtype in subtypes:
            decoded_list.append(
                serialization._decode_tree(raw_bytes, subtype, get_by_uuid)
            )
        return tuple(decoded_list)

    @staticmethod
    def encode(out, items, *, serialization, subtypes):
        if len(items) != len(subtypes):
            raise EncodeError("length of tuple does not match subtype count")
        for item, subtype in zip(items, subtypes):
            serialization._encode_tree(out, item, subtype)


class StringCodec(Codec):
    """A Codec for strings."""

    @staticmethod
    def decode(
        raw_bytes, *, serialization=None, subtypes=tuple(), get_by_uuid=None
    ):
        if subtypes != tuple():
            raise DecodeError("string should have no subtypes")
        size = Uint64Codec.decode(raw_bytes)
        return str(raw_bytes.read(size), "utf-8")

    @staticmethod
    def encode(out, val, *, serialization=None, subtypes=tuple()):
        if subtypes != ():
            raise EncodeError("string should have no subtypes")
        Uint64Codec.encode(out, len(val))
        out.write(val.encode())


class Uint64Codec(Codec):
    """A Codec for 64-bit unsigned integers."""

    @staticmethod
    def decode(
        raw_bytes, *, serialization=None, subtypes=tuple(), get_by_uuid=None
    ):
        if subtypes != ():
            raise DecodeError("uint64_t should have no subtypes")
        return int.from_bytes(
            raw_bytes.read(8), byteorder="little", signed=False
        )

    @staticmethod
    def encode(out, val, *, serialization=None, subtypes=tuple()):
        if subtypes != ():
            raise EncodeError("uint64_t should have no subtypes")
        out.write(val.to_bytes(8, byteorder="little"))


class UUIDCodec(Codec):
    """A Codec for raw UUIDs or Nodes.

    Decoding a UUID first checks the Node cache for an object with the
    corresponding UUID, and either returns the object it hits or a new
    raw UUID.
    """

    @staticmethod
    def decode(
        raw_bytes, *, serialization=None, subtypes=tuple(), get_by_uuid=None
    ):
        if subtypes != ():
            raise DecodeError("UUID should have no subtypes")
        uuid = UUID(bytes=raw_bytes.read(16))
        existing_node = get_by_uuid(uuid)
        return uuid if existing_node is None else existing_node

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


class UnknownData(bytes):
    """This class is a blob of bytes representing data with an unknown type.
    Generated by :func:`gtirb.Serialization.decode` when it encounters
    the name of an unknown codec. Use only at the top level of an auxdata.
    """


class Serialization:
    """Manages codecs used to serialize and deserialize GTIRB objects.

    The :meth:`gtirb.Serialization.decode` method of
    :attr:`gtirb.AuxData.serializer` is called when GTIRB AuxData is loaded via
    :meth:`gtirb.IR.load_protobuf`, and the :meth:`gtirb.Serialization.encode`
    method of :attr:`gtirb.AuxData.serializer` is called when GTIRB AuxData is
    saved to file via :meth:`gtirb.IR.save_protobuf`. You can alter the
    encoding and decoding of AuxData values via
    :attr:`gtirb.Serialization.codecs`. To do this, create a new subclass of
    :class:`gtirb.serialization.Codec` and add it to
    :attr:`gtirb.Serialization.codecs`:

    >>> gtirb.AuxData.serializer.codecs['my_custom_type'] = MyCustomCodec

    This example registers a new type name, ``my_custom_type``, and associate
    it with a new codec, ``MyCustomCodec``.

    :ivar ~.codecs: A mapping of type names to codecs. Codecs can be added
        or overridden using this dictionary.
    """

    def __init__(self):
        """Initialize with the built-in `gtirb.serialization.Codec` subclasses.
        """

        self.codecs = {
            "Addr": Uint64Codec,
            "Offset": OffsetCodec,
            "int64_t": Int64Codec,
            "mapping": MappingCodec,
            "sequence": SequenceCodec,
            "set": SetCodec,
            "string": StringCodec,
            "tuple": TupleCodec,
            "uint64_t": Uint64Codec,
            "UUID": UUIDCodec,
        }  # type: typing.Dict[str, Codec]

    def _decode_tree(self, raw_bytes, type_tree, get_by_uuid):
        # type: (typing.BinaryIO, SubtypeTree, CacheLookupFn) -> typing.Any
        """Decode the data in ``raw_bytes`` given a parsed type tree.

        :param raw_bytes: The binary stream to read bytes from.
        :param type_tree: The parsed type of the object encoded by
            ``raw_bytes``.
        """

        try:
            type_name, subtypes = type_tree
        except ValueError:
            raise DecodeError("could not unpack type tree %s" % str(type_tree))
        if type_name not in self.codecs:
            raise UnknownCodecError(type_name)
        codec = self.codecs[type_name]
        return codec.decode(
            raw_bytes,
            serialization=self,
            subtypes=subtypes,
            get_by_uuid=get_by_uuid,
        )

    def _encode_tree(self, out, val, type_tree):
        # type: (typing.BinaryIO, typing.Any, SubtypeTree) -> None
        """Encode the data in ``val`` given a parsed type tree.

        :param out: A binary stream to write bytes to.
        :param val: The :class:`gtirb.AuxData` to encode.
        :param type_tree: The parsed type to encode ``val`` as.
        """

        try:
            type_name, subtypes = type_tree
        except ValueError:
            raise EncodeError("could not unpack type tree %s" % str(type_tree))
        if type_name not in self.codecs:
            raise UnknownCodecError(type_name)
        codec = self.codecs[type_name]
        return codec.encode(out, val, serialization=self, subtypes=subtypes)

    @staticmethod
    def _parse_type(type_name):
        # type: (str) -> SubtypeTree
        """Given an encoded aux_data type_name, generate its parse tree.

        >>> _parse_type('foo')
        ('foo', ())

        >>> _parse_type('foo<bar>')
        ('foo', (('bar',()),))

        >>> _parse_type('foo<bar<baz>>')
        ('foo', (('bar', (('baz', ()),)),))

        :param type_name: The type name to parse into a ``SubtypeTree``.
        """
        tokens = findall("[^<>,]+|<|>|,", type_name)

        def parse(tokens, tree):
            tree = list(tree)
            # It is an error to parse nothing
            if len(tokens) == 0:
                raise TypeNameError(type_name)
            first_token, *tail = tokens

            # The first token should be a name
            if first_token in {"<", ">", ","}:
                raise TypeNameError(type_name)

            # Base case
            if len(tail) == 0:
                tree.append((first_token, ()))
                return tuple(tree), []
            next_token, *tail = tail

            # No subtypes
            if next_token == ",":
                tree.append((first_token, ()))

            # Parse subtypes
            if next_token == "<":
                # Extract just the subtype tokens and parse them
                stack = ["<"]
                subtype_tokens = list()
                remaining_tokens = list()
                for t in tail:
                    if len(stack) == 0:
                        remaining_tokens.append(t)
                        continue
                    if t == "<":
                        stack.append(t)
                    elif t == ">":
                        stack.pop()
                    subtype_tokens.append(t)
                if len(stack) > 0 or subtype_tokens[-1] != ">":
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
            if next_token == ",":
                return parse(tail, tree)

            # None of the rules match, error
            raise TypeNameError(type_name)

        # There should only be one item at the root of the tree
        try:
            (parse_tree,) = parse(tokens, [])[0]
        except ValueError:
            raise TypeNameError(type_name)
        return parse_tree

    def decode(
        self,
        raw_bytes,  # type: typing.BinaryIO
        type_name,  # type: str
        get_by_uuid=None,  # type: CacheLookupFn
    ):
        # type: (...) -> typing.Any
        """Decode a :class:`gtirb.AuxData` of the specified type
        from the specified byte stream.

        :param raw_bytes: The byte stream from which to read the encoded value.
        :param type_name: The type name of the object encoded by ``raw_bytes``.
        :param get_by_uuid: A function to look up nodes by UUID.
        :returns: The object encoded by ``raw_bytes``.
        """

        parse_tree = Serialization._parse_type(type_name)
        all_bytes = None
        if isinstance(raw_bytes, (bytes, bytearray, memoryview)):
            all_bytes = raw_bytes
        else:
            all_bytes = raw_bytes.read()
        try:
            return self._decode_tree(
                io.BytesIO(all_bytes), parse_tree, get_by_uuid
            )
        except UnknownCodecError:
            # we found an unknwon codec; the entire data structure can't be
            # parsed; return a blob of bytes
            return UnknownData(all_bytes)

    def encode(self, out, val, type_name):
        # type: (typing.BinaryIO,typing.Any,str) -> None
        """Encodes the value of an AuxData value to bytes.

        :param out: A binary stream to write bytes to.
        :param val: The :class:`gtirb.AuxData` to encode.
        :param type_name: The type name of the value encapsulated
            by the :class:`gtirb.AuxData`.
        """

        if isinstance(val, UnknownData):
            # it was a blob of bytes because of a decoding problem;
            # just write the whole thing out
            out.write(val)
            return
        parse_tree = Serialization._parse_type(type_name)
        try:
            self._encode_tree(out, val, parse_tree)
        except UnknownCodecError as e:
            # rethrow UnknownCodecError, because we were supposed to catch it
            # via UnknownData. This means the user manually wrote a bad type.
            raise EncodeError("unknown codec: %s" % e.name)
