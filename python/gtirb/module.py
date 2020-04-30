import itertools
import typing
from enum import Enum
from uuid import UUID

from .auxdata import AuxData, AuxDataContainer
from .block import ByteBlock, CfgNode, CodeBlock, DataBlock, ProxyBlock
from .byteinterval import ByteInterval, SymbolicExpressionElement
from .node import Node
from .proto import Module_pb2
from .section import Section
from .symbol import Symbol
from .util import (
    DictLike,
    SetWrapper,
    nodes_at,
    nodes_on,
    symbolic_expressions_at,
)


class Module(AuxDataContainer):
    """Represents a loadable object, such as an executable or library.

    :ivar ~.binary_path: The path to the loadable binary object
        represented by this module. An empty string if not specified.
        The file represented by this path is indicitave of what file
        this ``Module`` was initially created from; it is not guaranteed to
        currently exist or have the same contents.
    :ivar ~.isa: The ISA of the binary.
    :ivar ~.file_format: The file format of the binary.
    :ivar ~.name: The name given to the binary. Some file formats use this
        for linking and/or symbol resolution purposes. An empty string if
        not specified by the format.
    :ivar ~.preferred_addr: The preferred loading address of the binary.
    :ivar ~.proxies: A set containing all the :class:`gtirb.ProxyBlock`\\s
        in the binary.
    :ivar ~.rebase_delta: The rebase delta of the binary.
    :ivar ~.sections: A set containing all the :class:`gtirb.Section`\\s
        in the binary.
    :ivar ~.symbols: A set containing all the :class:`gtirb.Symbol`\\s
        in the binary.
    :ivar ~.entry_point: A :class:`CodeBlock` representing where
        control flow of this module begins at, or None if not present.
    """

    class FileFormat(Enum):
        """Identifies the executable file format of the binary represented
        by a :class:`gtirb.Module`.
        """

        Undefined = Module_pb2.FileFormat.Value("Format_Undefined")
        """A file format that has not yet been specified.
        This is for unitialized modules; do not use to refer to
        file formats without ``FileFormat`` values.
        """

        COFF = Module_pb2.FileFormat.Value("COFF")
        """The Common Object File Format."""

        ELF = Module_pb2.FileFormat.Value("ELF")
        """The Executable and Linkable Format,
        formerly the Extensible Linking Format.
        """

        IdaProDb32 = Module_pb2.FileFormat.Value("IdaProDb32")
        """A 32-bit IDA Pro database file."""

        IdaProDb64 = Module_pb2.FileFormat.Value("IdaProDb64")
        """A 64-bit IDA Pro database file."""

        MACHO = Module_pb2.FileFormat.Value("MACHO")
        """A Mach object file."""

        PE = Module_pb2.FileFormat.Value("PE")
        """Microsoft's Portable Executable format."""

        RAW = Module_pb2.FileFormat.Value("RAW")
        """A raw binary file, with no file format."""

        XCOFF = Module_pb2.FileFormat.Value("XCOFF")
        """The Extended Common Object File Format."""

    class ISA(Enum):
        """Identifies the instruction set architecture (ISA)
        targeted by a :class:`gtirb.Module`.
        """

        Undefined = Module_pb2.ISA.Value("ISA_Undefined")
        """An ISA that has not yet been specified.
        This is for unitialized modules;
        use :class:`gtirb.Module.ISA.ValidButUnsupported`
        instead for specifying undefined ISAs.
        """

        ARM = Module_pb2.ISA.Value("ARM")
        """The Acorn RISC Machine, 32-bit."""

        ARM64 = Module_pb2.ISA.Value("ARM64")
        """The Acorn RISC Machine, 64-bit."""

        IA32 = Module_pb2.ISA.Value("IA32")
        """The 32-bit Intel Architecture. Also known as i386, x86, or x32."""

        PPC32 = Module_pb2.ISA.Value("PPC32")
        """IBM's 32-bit PowerPC (Performance Optimization with Enhanced RISC /
        Performance Computing) architecture."""

        PPC64 = Module_pb2.ISA.Value("PPC64")
        """IBM's 64-bit PowerPC (Performance Optimization with Enhanced RISC /
        Performance Computing) architecture."""

        MIPS32 = Module_pb2.ISA.Value("MIPS32")
        """Microprocessor without Interlocked Pipelined Stages, 32-bit."""

        MIPS64 = Module_pb2.ISA.Value("MIPS64")
        """Microprocessor without Interlocked Pipelined Stages, 64-bit."""

        X64 = Module_pb2.ISA.Value("X64")
        """The 64-bit Intel Architecture. Also known as x86_64."""

        ValidButUnsupported = Module_pb2.ISA.Value("ValidButUnsupported")
        """An unknown or undefined ISA."""

    class _NodeSet(SetWrapper):
        def __init__(self, node, field, *args):
            self._node = node  # type: Module
            self._field = field  # type: str
            super().__init__(*args)

        def add(self, v):
            if v._module is not None:
                getattr(v._module, self._field).discard(v)
            v._module = self._node
            if self._node.ir is not None:
                v._add_to_uuid_cache(self._node.ir._local_uuid_cache)
            return super().add(v)

        def discard(self, v):
            if v not in self:
                return
            v._module = None
            if self._node.ir is not None:
                v._remove_from_uuid_cache(self._node.ir._local_uuid_cache)
            return super().discard(v)

    def __init__(
        self,
        *,
        aux_data=dict(),  # type: DictLike[str, AuxData]
        binary_path="",  # type: str
        file_format=FileFormat.Undefined,  # type: Module.FileFormat
        isa=ISA.Undefined,  # type: Module.ISA
        name="",  # type: str
        preferred_addr=0,  # type: int
        proxies=set(),  # type: typing.Iterable[ProxyBlock]
        rebase_delta=0,  # type: int
        sections=set(),  # type: typing.Iterable[Section]
        symbols=set(),  # type: typing.Iterable[Symbol]
        entry_point=None,  # type: typing.Optional[CodeBlock]
        uuid=None  # type: typing.Optional[UUID]
    ):
        # type: (...) -> None
        """
        :param aux_data: The initial auxiliary data to be associated
            with the object, as a mapping from names to
            :class:`gtirb.AuxData`, defaults to an empty :class:`dict`.
        :param binary_path: The path to the loadable binary object
            represented by this module.
        :param isa: The ISA of the binary.
        :param file_format: The file format of the binary.
        :param name: The name given to the binary.
        :param preferred_addr: The preferred loading address of the binary.
        :param proxies: A set containing all the :class:`gtirb.ProxyBlock`\\s
            in the binary.
        :param rebase_delta: The rebase delta of the binary.
        :param sections: A set containing all the :class:`gtirb.Section`\\s
            in the binary.
        :param symbols: A set containing all the :class:`gtirb.Symbol`\\s
            in the binary.
        :param entry_point: A :class:`CodeBlock` representing where
            control flow of this module begins at, or None if not present.
        :param uuid: The UUID of this ``Module``,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        """

        self._ir = None  # type: "IR"
        self.binary_path = binary_path  # type: str
        self.isa = isa  # type: Module.ISA
        self.file_format = file_format  # type: Module.FileFormat
        self.name = name  # type: str
        self.preferred_addr = preferred_addr  # type: int
        self.proxies = Module._NodeSet(
            self, "proxies", proxies
        )  # type: typing.Set[ProxyBlock]
        self.rebase_delta = rebase_delta  # type: int
        self.sections = Module._NodeSet(
            self, "sections", sections
        )  # type: typing.Set[Section]
        self.symbols = Module._NodeSet(
            self, "symbols", symbols
        )  # type: typing.Set[Symbol]
        self.entry_point = entry_point  # type: typing.Optional[CodeBlock]
        # Initialize the aux data last so that the cache is populated
        super().__init__(aux_data, uuid)

    @classmethod
    def _decode_protobuf(cls, proto_module, uuid, ir):
        # type: (Module_pb2.Module. UUID, typing.Optiona["IR"]) -> Module
        m = cls(
            binary_path=proto_module.binary_path,
            isa=Module.ISA(proto_module.isa),
            file_format=Module.FileFormat(proto_module.file_format),
            name=proto_module.name,
            preferred_addr=proto_module.preferred_addr,
            rebase_delta=proto_module.rebase_delta,
            uuid=uuid,
        )
        m._add_to_uuid_cache(ir._local_uuid_cache)

        # proxies depend on nothing
        m.proxies.update(
            ProxyBlock._from_protobuf(p, ir) for p in proto_module.proxies
        )
        # sections depend on symbolic expressions, so that step is split out
        # from _decode_protobuf into _decode_symbolic_expressions
        m.sections.update(
            Section._from_protobuf(s, ir) for s in proto_module.sections
        )
        # entry point is a code block, which depends on sections
        m.entry_point = (
            ir.get_by_uuid(UUID(bytes=proto_module.entry_point))
            if proto_module.entry_point
            else None
        )
        # symbols depend on blocks
        m.symbols.update(
            Symbol._from_protobuf(s, ir) for s in proto_module.symbols
        )
        # symbolic expressions depend on symbols
        for section in m.sections:
            for interval in section.byte_intervals:
                interval._decode_symbolic_expressions(ir)
        # aux data may depend on any node
        m.aux_data.update(
            AuxDataContainer._read_protobuf_aux_data(proto_module, ir)
        )

        return m

    def _to_protobuf(self):
        # type: () -> Module_pb2.Module
        proto_module = Module_pb2.Module()
        self._write_protobuf_aux_data(proto_module)
        proto_module.binary_path = self.binary_path
        proto_module.isa = self.isa.value
        proto_module.file_format = self.file_format.value
        proto_module.name = self.name
        proto_module.preferred_addr = self.preferred_addr
        proto_module.proxies.extend(p._to_protobuf() for p in self.proxies)
        proto_module.rebase_delta = self.rebase_delta
        proto_module.sections.extend(s._to_protobuf() for s in self.sections)
        proto_module.symbols.extend(s._to_protobuf() for s in self.symbols)
        if self.entry_point is not None:
            proto_module.entry_point = self.entry_point.uuid.bytes
        proto_module.uuid = self.uuid.bytes
        return proto_module

    def deep_eq(self, other):
        # type: (typing.Any) -> bool
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not super().deep_eq(other):
            return False
        if not isinstance(other, Module):
            return False
        for attr in (
            "binary_path",
            "isa",
            "file_format",
            "name",
            "preferred_addr",
            "rebase_delta",
        ):
            if getattr(self, attr) != getattr(other, attr):
                return False

        for attr in ("proxies", "sections", "symbols"):
            self_nodes = sorted(getattr(self, attr), key=lambda n: n.uuid)
            other_nodes = sorted(getattr(other, attr), key=lambda n: n.uuid)
            if not len(self_nodes) == len(other_nodes):
                return False
            for self_node, other_node in zip(self_nodes, other_nodes):
                if not self_node.deep_eq(other_node):
                    return False

        if self.entry_point is None:
            if other.entry_point is not None:
                return False
        else:
            if not self.entry_point.deep_eq(other.entry_point):
                return False

        return True

    def __repr__(self):
        # type: () -> str
        return (
            "Module("
            "uuid={uuid!r}, "
            "name={name!r}, "
            "binary_path={binary_path!r}, "
            "isa=Module.{isa!s}, "
            "file_format=Module.{file_format!s}, "
            "preferred_addr={preferred_addr:#x}, "
            "rebase_delta={rebase_delta:#x}, "
            "proxies={proxies!r}, "
            "sections={sections!r}, "
            "symbols={symbols!r}, "
            "entry_point={entry_point!r}, "
            ")".format(**self.__dict__)
        )

    @property
    def ir(self):
        # type: () -> typing.Optional["IR"]
        """The :class:`IR` this module belongs to."""

        return self._ir

    @ir.setter
    def ir(self, value):
        # type: (typing.Optional["IR"]) -> None
        if self._ir is not None:
            self._ir.modules.remove(self)
        if value is not None:
            value.modules.append(self)

    @property
    def address(self):
        # type: () -> typing.Optional[int]
        """Get the address of this module, if known.

        The address is calculated from the :class:`Section` objects in
        this module. More specifically, if the address of all sections
        in this module are fixed, then it will return the address of the
        section lowest in memory. If any one section does not have an address,
        then this will be ``None``, as the address is not calculable in that
        case. Note that a module with no sections in it has no address or
        size, so it will be ``None`` in that case.
        """

        lowest = None
        for s in self.sections:
            if s.address is None:
                return None
            if lowest is None or s.address < lowest:
                lowest = s.address
        return lowest

    @property
    def size(self):
        # type: () -> typing.Optional[int]
        """Get the size of this module, if known.

        The address is calculated from the :class:`Section` objects in
        this module. More specifically, if the address of all sections
        in this module are fixed, then it will return the difference between
        the lowest and highest address among the sections. If any one section
        does not have an address, then this will be ``None``, as the size is
        not calculable in that case. Note that a module with no sections in
        it has no address or size, so it will be ``None`` in that case.
        """

        lowest = None
        highest = None
        for s in self.sections:
            if s.address is None:
                return None
            if lowest is None or s.address < lowest:
                lowest = s.address
            if highest is None or s.address + s.size > highest:
                highest = s.address + s.size
        if lowest is None or highest is None:
            return None
        return highest - lowest

    @property
    def byte_intervals(self):
        # type: () -> typing.Iterator[ByteInterval]
        """The :class:`ByteInterval`\\s in this module."""

        return itertools.chain.from_iterable(
            s.byte_intervals for s in self.sections
        )

    @property
    def byte_blocks(self):
        # type: () -> typing.Iterator[ByteBlock]
        """The :class:`ByteBlock`\\s in this module."""

        return itertools.chain.from_iterable(
            s.byte_blocks for s in self.sections
        )

    @property
    def code_blocks(self):
        # type: () -> typing.Iterator[CodeBlock]
        """The :class:`CodeBlock`\\s in this module."""

        return itertools.chain.from_iterable(
            s.code_blocks for s in self.sections
        )

    @property
    def data_blocks(self):
        # type: () -> typing.Iterator[DataBlock]
        """The :class:`DataBlock`\\s in this module."""

        return itertools.chain.from_iterable(
            s.data_blocks for s in self.sections
        )

    @property
    def cfg_nodes(self):
        # type: () -> typing.Iterator[CfgNode]
        """The :class:`CfgNode`\\s in this module."""

        return itertools.chain(self.code_blocks, self.proxies)

    def sections_on(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[Section]
        """Finds all the sections that overlap an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_on(self.sections, addrs)

    def sections_at(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[Section]
        """Finds all the sections that begin at an address or range of
        addresses.

        :param addrs: Either a ``range`` object or a single address.
        """

        return nodes_at(self.sections, addrs)

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

        return symbolic_expressions_at(self.sections, addrs)

    def _add_to_uuid_cache(self, cache):
        # type: (typing.Dict[UUID, Node]) -> None
        """Update the UUID cache when this node is added."""

        cache[self.uuid] = self
        for proxy in self.proxies:
            proxy._add_to_uuid_cache(cache)
        for section in self.sections:
            section._add_to_uuid_cache(cache)
        for symbol in self.symbols:
            symbol._add_to_uuid_cache(cache)

    def _remove_from_uuid_cache(self, cache):
        # type: (typing.Dict[UUID, Node]) -> None
        """Update the UUID cache when this node is removed."""

        del cache[self.uuid]
        for proxy in self.proxies:
            proxy._remove_from_uuid_cache(cache)
        for section in self.sections:
            section._remove_from_uuid_cache(cache)
        for symbol in self.symbols:
            symbol._remove_from_uuid_cache(cache)
