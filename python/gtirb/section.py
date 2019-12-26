import Section_pb2
import typing
from uuid import UUID

from .node import Node
from .byteinterval import ByteInterval
from .util import SetWrapper


class Section(Node):
    """Represents a named section of the binary.

    Does not directly store the contents of the section, which are
    kept in a :class:`gtirb.ImageByteMap`.

    :ivar name: The section name (E.g. ".text", ".bss", etc).
    :ivar size: The size of this section in bytes.
    :ivar byte_intervals: The :class:`ByteInterval`\\s in this section.
    """

    class _ByteIntervalSet(SetWrapper):
        def add(self, v):
            if v._section is not None:
                v._section.byte_intervals.discard(v)
            v._section = self
            return super().add(v)

        def discard(self, v):
            v._section = None
            return super().discard(v)

    def __init__(
        self,
        *,
        name="",  # type: str
        size=0,  # type: int
        byte_intervals=(),  # type: typing.Iterable[ByteInterval]
        uuid=None,  # type: typing.Optional[UUID]
    ):
        """
        :param name: The name of this section.
        :param size: The size of this section in bytes.
        :param byte_intervals: The :class:`ByteInterval`\\s in this section.
        :param uuid: The UUID of this ``Section``,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        """

        super().__init__(uuid)
        self.name = name  # type: str
        self.size = size  # type: int
        self.byte_intervals = Section._ByteIntervalSet(
            byte_intervals
        )  # type: typing.Set[ByteInterval]
        self._module = None  # type: "Module"

    @classmethod
    def _decode_protobuf(cls, proto_section, uuid):
        # type: (Section_pb2.Section, UUID) -> Section
        return cls(
            name=proto_section.name,
            size=proto_section.size,
            byte_intervals=(
                ByteInterval._from_protobuf(bi)
                for bi in proto_section.byte_intervals
            ),
            uuid=uuid,
        )

    def _to_protobuf(self):
        # type: () -> Section_pb2.Section
        """Get a Protobuf representation of ``self``."""

        proto_section = Section_pb2.Section()
        proto_section.uuid = self.uuid.bytes
        proto_section.name = self.name
        proto_section.size = self.size
        proto_section.byte_intervals.extend(
            bi._to_protobuf() for bi in self.byte_intervals
        )
        return proto_section

    def deep_eq(self, other):
        # type: (typing.Any) -> bool
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, Section):
            return False
        return (
            self.uuid == other.uuid
            and self.name == other.name
            and self.size == other.size
            and len(self.byte_intervals) == len(other.byte_intervals)
            and all(
                self_node.deep_eq(other_node)
                for self_node, other_node in zip(
                    self.byte_intervals, other.byte_intervals
                )
            )
        )

    def __repr__(self):
        # type: () -> str
        return (
            "Section("
            "uuid={uuid!r}, "
            "name={name!r}, "
            "size={size!r}, "
            "byte_intervals={byte_intervals!r}, "
            ")".format(**self.__dict__)
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
