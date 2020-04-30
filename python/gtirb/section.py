import itertools
import typing
from enum import Enum
from uuid import UUID

from .block import ByteBlock, CodeBlock, DataBlock
from .byteinterval import ByteInterval, SymbolicExpressionElement
from .node import Node
from .proto import Section_pb2
from .util import SetWrapper, nodes_at, nodes_on, symbolic_expressions_at


class Section(Node):
    """Represents a named section of the binary.

    Does not directly store the contents of the section, which are
    kept in a :class:`gtirb.ImageByteMap`.

    :ivar ~.name: The section name (E.g. ".text", ".bss", etc).
    :ivar ~.byte_intervals: The :class:`ByteInterval`\\s in this section.
    :ivar ~.flags: The :class:`Section.Flag`\\s this section has.
    """

    class Flag(Enum):
        """A flag representing a known property of a section."""

        Undefined = Section_pb2.SectionFlag.Value("Section_Undefined")
        """This value is defined for Protobuf compatibility. Do not use."""

        Readable = Section_pb2.SectionFlag.Value("Readable")
        """This section can be read from at runtime."""

        Writable = Section_pb2.SectionFlag.Value("Writable")
        """This section can be written to at runtime."""

        Executable = Section_pb2.SectionFlag.Value("Executable")
        """This section contains executable code."""

        Loaded = Section_pb2.SectionFlag.Value("Loaded")
        """This section is present in memory at runtime."""

        Initialized = Section_pb2.SectionFlag.Value("Initialized")
        """This section has bytes allocated to it in the binary file."""

        ThreadLocal = Section_pb2.SectionFlag.Value("ThreadLocal")
        """This section is created in memory once per thread."""

    class _ByteIntervalSet(SetWrapper):
        def __init__(self, node, *args):
            self._node = node  # type: Section
            super().__init__(*args)

        def add(self, v):
            if v._section is not None:
                v._section.byte_intervals.discard(v)
            v._section = self._node
            if self._node.ir is not None:
                v._add_to_uuid_cache(self._node.ir._local_uuid_cache)
            return super().add(v)

        def discard(self, v):
            if v not in self:
                return
            v._section = None
            if self._node.ir is not None:
                v._remove_from_uuid_cache(self._node.ir._local_uuid_cache)
            return super().discard(v)

    def __init__(
        self,
        *,
        name="",  # type: str
        byte_intervals=(),  # type: typing.Iterable[ByteInterval]
        flags=set(),  # type: typing.Iterable[Section.Flag]
        uuid=None  # type: typing.Optional[UUID]
    ):
        """
        :param name: The name of this section.
        :param byte_intervals: The :class:`ByteInterval`\\s in this section.
        :param flags: The :class:`Section.Flag`\\s this section has.
        :param uuid: The UUID of this ``Section``,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        """

        super().__init__(uuid)
        self._module = None  # type: "Module"
        self.name = name  # type: str
        self.byte_intervals = Section._ByteIntervalSet(
            self, byte_intervals
        )  # type: typing.Set[ByteInterval]
        self.flags = set(flags)  # type: typing.Set[Section.Flag]

    @classmethod
    def _decode_protobuf(cls, proto_section, uuid, ir):
        # type: (Section_pb2.Section, UUID, typing.Optional["IR"]) -> Section
        s = cls(
            name=proto_section.name,
            flags=(Section.Flag(f) for f in proto_section.section_flags),
            uuid=uuid,
        )
        s._add_to_uuid_cache(ir._local_uuid_cache)
        s.byte_intervals.update(
            ByteInterval._from_protobuf(bi, ir)
            for bi in proto_section.byte_intervals
        )
        return s

    def _to_protobuf(self):
        # type: () -> Section_pb2.Section
        """Get a Protobuf representation of ``self``."""

        proto_section = Section_pb2.Section()
        proto_section.uuid = self.uuid.bytes
        proto_section.name = self.name
        proto_section.byte_intervals.extend(
            bi._to_protobuf() for bi in self.byte_intervals
        )
        proto_section.section_flags.extend(f.value for f in self.flags)
        return proto_section

    def deep_eq(self, other):
        # type: (typing.Any) -> bool
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, Section):
            return False
        return (
            self.uuid == other.uuid
            and self.name == other.name
            and len(self.byte_intervals) == len(other.byte_intervals)
            and all(
                self_node.deep_eq(other_node)
                for self_node, other_node in zip(
                    sorted(self.byte_intervals, key=lambda bi: bi.uuid),
                    sorted(other.byte_intervals, key=lambda bi: bi.uuid),
                )
            )
            and self.flags == other.flags
        )

    def __repr__(self):
        # type: () -> str
        return (
            "Section("
            "uuid={uuid!r}, "
            "name={name!r}, "
            "byte_intervals={byte_intervals!r}, "
            "flags=({pretty_flags}), "
            ")".format(
                pretty_flags=", ".join(
                    "Section." + str(f) for f in self.flags
                ),
                **self.__dict__,
            )
        )

    @property
    def module(self):
        # type: () -> "Module"
        """The :class:`Module` this section belongs to."""

        return self._module

    @module.setter
    def module(self, value):
        # type: ("Module") -> None
        if self._module is not None:
            self._module.sections.discard(self)
        if value is not None:
            value.sections.add(self)

    @property
    def byte_blocks(self):
        # type: () -> typing.Iterator[ByteBlock]
        """The :class:`ByteBlock`\\s in this section."""

        return itertools.chain.from_iterable(
            bi.blocks for bi in self.byte_intervals
        )

    @property
    def code_blocks(self):
        # type: () -> typing.Iterator[CodeBlock]
        """The :class:`CodeBlock`\\s in this section."""

        return (b for b in self.byte_blocks if isinstance(b, CodeBlock))

    @property
    def data_blocks(self):
        # type: () -> typing.Iterator[DataBlock]
        """The :class:`DataBlock`\\s in this section."""

        return (b for b in self.byte_blocks if isinstance(b, DataBlock))

    @property
    def address(self):
        # type: () -> typing.Optional[int]
        """Get the address of this section, if known.

        The address is calculated from the :class:`ByteInterval` objects in
        this section. More specifically, if the address of all byte intervals
        in this section are fixed, then it will return the address of the
        interval lowest in memory. If any one interval does not have an address
        then this will be ``None``, as the address is not calculable in that
        case. Note that a section with no intervals in it has no address or
        size, so it will be ``None`` in that case.
        """

        lowest = None
        for bi in self.byte_intervals:
            if bi.address is None:
                return None
            if lowest is None or bi.address < lowest:
                lowest = bi.address
        return lowest

    @property
    def size(self):
        # type: () -> typing.Optional[int]
        """Get the size of this section, if known.

        The address is calculated from the :class:`ByteInterval` objects in
        this section. More specifically, if the address of all byte intervals
        in this section are fixed, then it will return the difference between
        the lowest and highest address among the intervals. If any one interval
        does not have an address, then this will be ``None``, as the size is
        not calculable in that case. Note that a section with no intervals in
        it has no address or size, so it will be ``None`` in that case.
        """

        lowest = None
        highest = None
        for bi in self.byte_intervals:
            if bi.address is None:
                return None
            if lowest is None or bi.address < lowest:
                lowest = bi.address
            if highest is None or bi.address + bi.size > highest:
                highest = bi.address + bi.size
        if lowest is None or highest is None:
            return None
        return highest - lowest

    def byte_intervals_on(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[ByteInterval]
        """Finds all the byte intervals that overlap an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_on(self.byte_intervals, addrs)

    def byte_intervals_at(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[ByteInterval]
        """Finds all the byte intervals that begin at an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_at(self.byte_intervals, addrs)

    def byte_blocks_on(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[ByteBlock]
        """Finds all the byte blocks that overlap an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_on(self.byte_blocks, addrs)

    def byte_blocks_at(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[ByteBlock]
        """Finds all the byte blocks that begin at an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_at(self.byte_blocks, addrs)

    def code_blocks_on(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[CodeBlock]
        """Finds all the code blocks that overlap an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_on(self.code_blocks, addrs)

    def code_blocks_at(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[CodeBlock]
        """Finds all the code blocks that begin at an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_at(self.code_blocks, addrs)

    def data_blocks_on(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[DataBlock]
        """Finds all the data blocks that overlap an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_on(self.data_blocks, addrs)

    def data_blocks_at(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[DataBlock]
        """Finds all the data blocks that begin at an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_at(self.data_blocks, addrs)

    def symbolic_expressions_at(
        self, addrs  # type: typing.Union[int, range]
    ):
        # type: (...) -> typing.Iterable[SymbolicExpressionElement]
        """Finds all the symbolic expressions that begin at an address or
        range of addresses.

        :param addrs: Either a ``range`` object or a single address.
        :returns: Yields ``(interval, offset, symexpr)`` tuples for every
            symbolic expression in the range.
        """

        return symbolic_expressions_at(self.byte_intervals, addrs)

    def _add_to_uuid_cache(self, cache):
        # type: (typing.Dict[UUID, Node]) -> None
        """Update the UUID cache when this node is added."""

        cache[self.uuid] = self
        for bi in self.byte_intervals:
            bi._add_to_uuid_cache(cache)

    def _remove_from_uuid_cache(self, cache):
        # type: (typing.Dict[UUID, Node]) -> None
        """Update the UUID cache when this node is removed."""

        del cache[self.uuid]
        for bi in self.byte_intervals:
            bi._remove_from_uuid_cache(cache)

    @property
    def ir(self):
        # type: () -> "IR"
        """Get the IR this node ultimately belongs to."""
        if self.module is None:
            return None
        return self.module.ir
