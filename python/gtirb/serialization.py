import io
from re import findall
from typing import (
    Any,
    BinaryIO,
    Callable,
    Collection,
    Dict,
    Iterable,
    Mapping,
    Optional,
    Sequence,
    Set,
    Tuple,
    Type,
    Union,
)
from uuid import UUID

from .node import Node
from .offset import Offset

CacheLookupFn = Optional[Callable[[UUID], Optional[Node]]]


class CodecError(Exception):
    """Base class for codec exceptions."""


class DecodeError(CodecError):
    """An exception during decoding."""


class EncodeError(CodecError):
    """An exception during encoding."""


class TypeNameError(EncodeError):
    """A type name is malformed."""

    def __init__(self, hint: str) -> None:
        super().__init__("malformed type name: '%s'" % (hint))


class UnknownCodecError(CodecError):
    """An unknown codec name is encountered.
    Caught and handled by the top-level codec methods.

    :param name: the name of the unknown codec
    """

    def __init__(self, name: str) -> None:
        self.name = name


class SubtypeTree:
    """A type hint representing a parsed serialization type name.
    A ``SubtypeTree`` is has two items: A ``str`` giving
    the name of the type and a Sequence of type parameters
    (which are also ``SubtypeTree``\\s). For example, the following are all
    valid ``SubtypeTree``\\s:

    >>> SubtypeTree('string', ())
    >>> SubtypeTree('sequence', (SubtypeTree('UUID',()),))
    >>> SubtypeTree(
        'mapping', (
            SubtypeTree('string', ()),
            SubtypeTree('set', (SubtypeTree('UUID', ()),))
        )
    )
    """

    def __init__(self, name: str, subtypes: Sequence["SubtypeTree"]) -> None:
        self.name = name
        self.subtypes = subtypes

    def __eq__(self, other: object) -> bool:
        if isinstance(other, SubtypeTree):
            return self.name == other.name and self.subtypes == other.subtypes
        if isinstance(other, tuple):
            return (self.name, self.subtypes) == other
        return False


class Variant:
    # Because the Variant can contain arbitrary data, depending on the context
    # in which it is used, it has type Any. This requires an exception to the
    # project-wide mypy configuration that disallows Any.
    def __init__(self, index: int, val: Any):  # type: ignore[misc]
        self.index = index
        self.val = val

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Variant):
            return self.index == other.index and self.val == other.val
        return False


class Codec:
    """The base class for codecs."""

    @staticmethod
    def decode(
        raw_bytes: BinaryIO,
        *,
        serialization: "Serialization",
        subtypes: Sequence[SubtypeTree],
        get_by_uuid: CacheLookupFn = None,
    ) -> object:
        """Decode the specified raw data into a Python object.

        :param raw_bytes: The BytesIO object to be decoded.
        :param serialization: A Serialization instance used to invoke
            other codecs if needed.
        :param subtypes: The parsed type of this object.
        :param get_by_uuid: A function to look up nodes by UUID.
        :returns: A new Python object, as decoded from ``raw_bytes``.
        """

        raise NotImplementedError  # pragma: no cover

    @staticmethod
    def encode(
        out: BinaryIO,
        item: object,
        *,
        serialization: "Serialization",
        subtypes: Sequence[SubtypeTree],
    ) -> None:
        """Encode an item, writing the serialized object to ``out``.

        :param out: A binary stream to serialize to.
        :param item: The arbitrary Python object to encode.
        :param serialization: A Serialization instance, used to invoke
            other codecs if needed.
        :param subtypes: The parsed type of this object.
        """

        raise NotImplementedError  # pragma: no cover


class MappingCodec(Codec):
    """A Codec for mapping<K,V> entries. Implemented via ``dict``."""

    @staticmethod
    def decode(
        raw_bytes: BinaryIO,
        *,
        serialization: "Serialization",
        subtypes: Sequence[SubtypeTree],
        get_by_uuid: Optional[CacheLookupFn] = None,
    ) -> Mapping[object, object]:
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
    def encode(
        out: BinaryIO,
        mapping: object,
        *,
        serialization: "Serialization",
        subtypes: Sequence[SubtypeTree],
    ) -> None:
        if not isinstance(mapping, Mapping):
            raise EncodeError("Mapping codec only supports Mappings")
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
        raw_bytes: BinaryIO,
        *,
        serialization: "Serialization" = None,
        subtypes: Sequence[SubtypeTree] = (),
        get_by_uuid: Optional[CacheLookupFn] = None,
    ) -> Offset:
        if subtypes != ():
            raise DecodeError("Offset should have no subtypes")
        element_uuid = UUIDCodec.decode(raw_bytes, get_by_uuid=get_by_uuid)
        displacement = Uint64Codec.decode(raw_bytes)

        return Offset(element_uuid, displacement)

    @staticmethod
    def encode(
        out: BinaryIO,
        val: object,
        *,
        serialization: "Serialization" = None,
        subtypes: Sequence[SubtypeTree] = (),
    ) -> None:
        if not isinstance(val, Offset):
            raise EncodeError("Offset codec only supports Offsets")
        if subtypes != ():
            raise EncodeError("Offset should have no subtypes")
        UUIDCodec.encode(out, val.element_id)
        Uint64Codec.encode(out, val.displacement)


class SequenceCodec(Codec):
    """A Codec for sequence<T> entries. Implemented via ``list``."""

    @staticmethod
    def decode(
        raw_bytes: BinaryIO,
        *,
        serialization: "Serialization",
        subtypes: Sequence[SubtypeTree],
        get_by_uuid: Optional[CacheLookupFn] = None,
    ) -> Sequence[object]:
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
    def encode(
        out: BinaryIO,
        sequence: object,
        *,
        serialization: "Serialization",
        subtypes: Sequence[SubtypeTree],
    ) -> None:
        if not isinstance(sequence, Sequence):
            raise EncodeError("Sequence codec only supports Collections")
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
    def decode(
        raw_bytes: BinaryIO,
        *,
        serialization: "Serialization",
        subtypes: Sequence[SubtypeTree],
        get_by_uuid: Optional[CacheLookupFn] = None,
    ) -> Set[object]:
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
    def encode(
        out: BinaryIO,
        items: object,
        *,
        serialization: "Serialization",
        subtypes: Sequence[SubtypeTree],
    ) -> None:
        if not isinstance(items, Collection):
            raise EncodeError("Set codec only supports Collections")
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
    def decode(
        raw_bytes: BinaryIO,
        *,
        serialization: "Serialization",
        subtypes: Sequence[SubtypeTree],
        get_by_uuid: Optional[CacheLookupFn] = None,
    ) -> Tuple[object, ...]:
        # The length of a tuple is not contained in the Protobuf
        # representation, so error checking cannot be done here.
        decoded_list = list()
        for subtype in subtypes:
            decoded_list.append(
                serialization._decode_tree(raw_bytes, subtype, get_by_uuid)
            )
        return tuple(decoded_list)

    @staticmethod
    def encode(
        out: BinaryIO,
        items: object,
        *,
        serialization: "Serialization",
        subtypes: Sequence[SubtypeTree],
    ) -> None:
        if not isinstance(items, Collection):
            raise EncodeError("Tuple codec only supports Collections")
        if len(items) != len(subtypes):
            raise EncodeError("length of tuple does not match subtype count")
        for item, subtype in zip(items, subtypes):
            serialization._encode_tree(out, item, subtype)


class StringCodec(Codec):
    """A Codec for strings."""

    @staticmethod
    def decode(
        raw_bytes: BinaryIO,
        *,
        serialization: "Serialization" = None,
        subtypes: Sequence[SubtypeTree] = (),
        get_by_uuid: Optional[CacheLookupFn] = None,
    ) -> str:
        if subtypes != tuple():
            raise DecodeError("string should have no subtypes")
        size = Uint64Codec.decode(raw_bytes)
        return raw_bytes.read(size).decode("utf-8")

    @staticmethod
    def encode(
        out: BinaryIO,
        val: object,
        *,
        serialization: "Serialization" = None,
        subtypes: Sequence[SubtypeTree] = (),
    ) -> None:
        if not isinstance(val, str):
            raise EncodeError("String codec only supports strings")
        if subtypes != ():
            raise EncodeError("string should have no subtypes")
        Uint64Codec.encode(out, len(val))
        out.write(val.encode())


class IntegerCodec(Codec):
    """Generic base class for integer-based Codecs"""

    typname: str
    bytesize: int
    signed: bool

    @classmethod
    def decode(
        cls,
        raw_bytes: BinaryIO,
        *,
        serialization: "Serialization" = None,
        subtypes: Sequence[SubtypeTree] = (),
        get_by_uuid: Optional[CacheLookupFn] = None,
    ) -> int:
        if subtypes != ():
            raise DecodeError(f"{cls.typname} should have no subtypes")
        return int.from_bytes(
            raw_bytes.read(cls.bytesize), byteorder="little", signed=cls.signed
        )

    @classmethod
    def encode(
        cls,
        out: BinaryIO,
        val: object,
        *,
        serialization: "Serialization" = None,
        subtypes: Sequence[SubtypeTree] = (),
    ) -> None:
        if not isinstance(val, int):
            raise EncodeError("Integer codec only supports integers")
        if subtypes != ():
            raise EncodeError(f"{cls.typname} should have no subtypes")
        out.write(
            val.to_bytes(cls.bytesize, byteorder="little", signed=cls.signed)
        )


class Uint64Codec(IntegerCodec):
    """A Codec for 64-bit unsigned integers."""

    typname = "uint64_t"
    bytesize = 8
    signed = False


class Uint32Codec(IntegerCodec):
    """A Codec for 32-bit unsigned integers."""

    typname = "uint32_t"
    bytesize = 4
    signed = False


class Uint16Codec(IntegerCodec):
    """A Codec for 16-bit unsigned integers."""

    typname = "uint16_t"
    bytesize = 2
    signed = False


class Uint8Codec(IntegerCodec):
    """A Codec for 8-bit unsigned integers."""

    typname = "uint8_t"
    bytesize = 1
    signed = False


class Int64Codec(IntegerCodec):
    """A Codec for 64-bit signed integers."""

    typname = "int64_t"
    bytesize = 8
    signed = True


class Int32Codec(IntegerCodec):
    """A Codec for 32-bit signed integers."""

    typname = "int32_t"
    bytesize = 4
    signed = True


class Int16Codec(IntegerCodec):
    """A Codec for 16-bit signed integers."""

    typname = "int16_t"
    bytesize = 2
    signed = True


class Int8Codec(IntegerCodec):
    """A Codec for 8-bit signed integers."""

    typname = "int8_t"
    bytesize = 1
    signed = True


class UUIDCodec(Codec):
    """A Codec for raw UUIDs or Nodes.

    Decoding a UUID first checks the Node cache for an object with the
    corresponding UUID, and either returns the object it hits or a new
    raw UUID.
    """

    @staticmethod
    def decode(
        raw_bytes: BinaryIO,
        *,
        serialization: "Serialization" = None,
        subtypes: Sequence[SubtypeTree] = (),
        get_by_uuid: Optional[CacheLookupFn] = None,
    ) -> Union[UUID, Node]:
        if subtypes != ():
            raise DecodeError("UUID should have no subtypes")
        uuid = UUID(bytes=raw_bytes.read(16))
        existing_node = None if get_by_uuid is None else get_by_uuid(uuid)
        return uuid if existing_node is None else existing_node

    @staticmethod
    def encode(
        out: BinaryIO,
        val: object,
        *,
        serialization: "Serialization" = None,
        subtypes: Sequence[SubtypeTree] = (),
    ) -> None:
        if subtypes != ():
            raise EncodeError("UUID should have no subtypes")
        if isinstance(val, Node):
            out.write(val.uuid.bytes)
        elif isinstance(val, UUID):
            out.write(val.bytes)
        else:
            raise EncodeError("UUID codec only supports UUIDs or Nodes")


class VariantCodec(Codec):
    """A Codec for variant<Ts...> entries.

    An encoded record containg two part:
    index - position of member of variant's list
    value - encoded values of selected member
    """

    @staticmethod
    def decode(
        raw_bytes: BinaryIO,
        *,
        serialization: "Serialization",
        subtypes: Sequence[SubtypeTree] = (),
        get_by_uuid: Optional[CacheLookupFn] = None,
    ) -> Variant:
        index = int.from_bytes(
            raw_bytes.read(8), byteorder="little", signed=False
        )
        val = serialization._decode_tree(
            raw_bytes, subtypes[index], get_by_uuid
        )
        return Variant(index, val)

    @staticmethod
    def encode(
        out: BinaryIO,
        variant: object,
        *,
        serialization: "Serialization",
        subtypes: Sequence[SubtypeTree] = (),
    ) -> None:
        if not isinstance(variant, Variant):
            raise EncodeError("Variant codec only supports variants")
        # variant is a named tuple containg index and value
        # writing the index
        out.write(variant.index.to_bytes(8, byteorder="little"))
        # writing the value
        serialization._encode_tree(out, variant.val, subtypes[variant.index])


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

    def __init__(self) -> None:
        """Initialize with the built-in `gtirb.serialization.Codec` subclasses.
        """

        self.codecs: Dict[str, Type[Codec]] = {
            "Addr": Uint64Codec,
            "Offset": OffsetCodec,
            "int64_t": Int64Codec,
            "int32_t": Int32Codec,
            "int16_t": Int16Codec,
            "int8_t": Int8Codec,
            "mapping": MappingCodec,
            "sequence": SequenceCodec,
            "set": SetCodec,
            "string": StringCodec,
            "tuple": TupleCodec,
            "uint64_t": Uint64Codec,
            "uint32_t": Uint32Codec,
            "uint16_t": Uint16Codec,
            "uint8_t": Uint8Codec,
            "UUID": UUIDCodec,
            "variant": VariantCodec,
        }

    def _decode_tree(
        self,
        raw_bytes: BinaryIO,
        type_tree: SubtypeTree,
        get_by_uuid: CacheLookupFn,
    ) -> object:
        """Decode the data in ``raw_bytes`` given a parsed type tree.

        :param raw_bytes: The binary stream to read bytes from.
        :param type_tree: The parsed type of the object encoded by
            ``raw_bytes``.
        """

        if type_tree.name not in self.codecs:
            raise UnknownCodecError(type_tree.name)
        codec = self.codecs[type_tree.name]
        return codec.decode(
            raw_bytes,
            serialization=self,
            subtypes=type_tree.subtypes,
            get_by_uuid=get_by_uuid,
        )

    def _encode_tree(
        self, out: BinaryIO, val: object, type_tree: SubtypeTree
    ) -> None:
        """Encode the data in ``val`` given a parsed type tree.

        :param out: A binary stream to write bytes to.
        :param val: The :class:`gtirb.AuxData` to encode.
        :param type_tree: The parsed type to encode ``val`` as.
        """

        if type_tree.name not in self.codecs:
            raise UnknownCodecError(type_tree.name)
        codec = self.codecs[type_tree.name]
        return codec.encode(
            out, val, serialization=self, subtypes=type_tree.subtypes
        )

    @staticmethod
    def _parse_type(type_name: str) -> SubtypeTree:
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

        def parse(
            tokens: Sequence[str], tree: Iterable[SubtypeTree],
        ) -> Tuple[Tuple[SubtypeTree, ...], Sequence[None]]:
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
                tree.append(SubtypeTree(first_token, ()))
                return tuple(tree), []
            next_token, *tail = tail

            # No subtypes
            if next_token == ",":
                tree.append(SubtypeTree(first_token, ()))

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
                tree.append(SubtypeTree(first_token, subtypes))
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
        raw_bytes: Union[bytes, bytearray, memoryview, BinaryIO],
        type_name: str,
        get_by_uuid: CacheLookupFn = None,
    ) -> object:
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

    def encode(self, out: BinaryIO, val: object, type_name: str) -> None:
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
