import typing
from enum import Enum
from uuid import UUID

from .block import CfgNode
from .node import Node
from .proto import CFG_pb2


class Edge:
    """An edge in the CFG from ``source`` to ``target``, with optional
    control-flow details in ``label``.

    :ivar ~.source: The source CFG node.
    :ivar ~.target: The target CFG node.
    :ivar ~.label: An optional label containing more control flow information.
    """

    class Type(Enum):
        """The type of control flow transfer indicated by a
        :class:`gtirb.Edge`.
        """

        Branch = CFG_pb2.EdgeType.Value("Type_Branch")
        """This edge is the explicit target of a jump instruction.
        May be conditional or unconditional. If conditional, there will be
        a corresponding edge of type :attr:`gtirb.Edge.Type.Fallthrough`.
        """

        Call = CFG_pb2.EdgeType.Value("Type_Call")
        """This edge is the explicit target of a call instruction.
        Unless the function does not return, there will also be a
        corresponding edge of type :attr:`gtirb.Edge.Type.Fallthrough`.
        """

        Fallthrough = CFG_pb2.EdgeType.Value("Type_Fallthrough")
        """This edge represents two blocks executing in sequence.
        This occurs on the non-branching paths of conditional branch
        instructions, after call instructons have returned, and when two
        blocks have no control flow between them, but another
        :class:`gtirb.Edge` targets the target block.
        If there exists a fallthrough edge from block ``A`` to block ``B``,
        then ``A`` must immediately precede ``B`` in memory.
        """

        Return = CFG_pb2.EdgeType.Value("Type_Return")
        """This edge represents a return from a function, generally via a
        return instruction. Return edges may be omitted from valid CFGs;
        a function may have an uncomputable number of possible return sites,
        due to the possibility of indirect calls.
        """

        Syscall = CFG_pb2.EdgeType.Value("Type_Syscall")
        """This edge is the explicit target of a system call instruction.
        Unless the function does not return, there will also be a
        corresponding edge of type :attr:`gtirb.Edge.Type.Fallthrough`. This
        is the system call equivalent to :class:`gtirb.Edge.Type.Call`.
        """

        Sysret = CFG_pb2.EdgeType.Value("Type_Sysret")
        """This edge represents a return from a system call, generally via a
        return instruction. Return edges may be omitted from valid CFGs;
        a function may have an uncomputable number of possible return sites,
        due to the possibility of indirect calls. This is the system call
        equivalent to :class:`gtirb.Edge.Type.Return`.
        """

    class Label:
        """Contains a more detailed description of a :class:`gtirb.Edge`
        in the CFG.

        :ivar ~.conditional: When this edge is part of a conditional branch,
            ``conditional`` is ``True`` when the edge represents the control
            flow taken when the branch's condition is met, and ``False``
            when it represents the control flow taken when the branch's
            condition is not met. Otherwise, it is always ``False``.
        :ivar ~.direct: ``True`` if the branch or call is direct,
                and ``False`` if it is indirect. If an edge is indirect,
                then all outgoing indirect edges represent the set of
                possible locations the edge may branch to. If there
                exists an indirect outgoing edge to a :class:`gtirb.ProxyBlock`
                without any :class:`gtirb.Symbol` objects referring to it,
                then the set of all possible branch locations is unknown.
        :ivar ~.type: The type of control flow the :class:`gtirb.Edge`
            represents.
        """

        def __init__(
            self,
            type,  # type: Edge.Type
            *,
            conditional=False,  # type: bool
            direct=True  # type: bool
        ):
            """
            :param type: The type of control flow the :class:`gtirb.Edge`
                represents.
            :param conditional: ``True`` when this edge represents a
                conditional branch taken when the branch's condition is met,
                and ``False`` otherwise.
            :param direct: ``True`` if the branch or call is direct,
                and ``False`` if it is indirect.
            """

            self.type = type  # type: Edge.Type
            self.conditional = conditional  # type: bool
            self.direct = direct  # type: bool

        @classmethod
        def _from_protobuf(cls, label):
            # type: (CFG_pb2.EdgeLabel) -> Edge.Label
            return Edge.Label(
                type=Edge.Type(label.type),
                conditional=label.conditional,
                direct=label.direct,
            )

        def _to_protobuf(self):
            # type: () -> CFG_pb2.EdgeLabel
            proto_label = CFG_pb2.EdgeLabel()
            proto_label.type = self.type.value
            proto_label.conditional = self.conditional
            proto_label.direct = self.direct
            return proto_label

        def __eq__(self, other):
            # type: (typing.Any) -> bool
            if not isinstance(other, Edge.Label):
                return False
            return (
                self.type == other.type
                and self.conditional == other.conditional
                and self.direct == other.direct
            )

        def __hash__(self):
            # type: () -> int
            return hash((self.type, self.conditional, self.direct))

        def __repr__(self):
            # type: () -> str
            return (
                "Edge.Label("
                "type=Edge.{type!s}, "
                "conditional={conditional!r}, "
                "direct={direct!r}, "
                ")".format(**self.__dict__)
            )

    def __init__(
        self,
        source,  # type: CfgNode
        target,  # type: CfgNode
        label=None,  # type: typing.Optional["Edge.Label"]
    ):
        # type: (...) -> None
        """
        :param source: The source CFG node.
        :param target: The target CFG node.
        :param label: An optional label
            containing more control flow information.
        """
        self.source = source  # type: CfgNode
        self.target = target  # type: CfgNode
        self.label = label  # type: typing.Optional["Edge.Label"]

    @classmethod
    def _from_protobuf(cls, edge, get_by_uuid):
        # type: (CFG_pb2.Edge, typing.Callable[[UUID], Node]) -> Edge
        source_uuid = UUID(bytes=edge.source_uuid)
        target_uuid = UUID(bytes=edge.target_uuid)

        source = get_by_uuid(source_uuid)
        target = get_by_uuid(target_uuid)
        if not isinstance(source, CfgNode):
            raise ValueError(
                "In CFG: source UUID %s is a %s, not a CfgNode"
                % (source_uuid, type(source).__name__)
            )
        if not isinstance(target, CfgNode):
            raise ValueError(
                "In CFG: target UUID %s is a %s, not a CfgNode"
                % (target_uuid, type(target).__name__)
            )

        label = None
        if edge.label is not None:
            label = Edge.Label._from_protobuf(edge.label)
        return cls(source, target, label)

    def _to_protobuf(self):
        # type: () -> CFG_pb2.Edge
        proto_edge = CFG_pb2.Edge()
        proto_edge.source_uuid = self.source.uuid.bytes
        proto_edge.target_uuid = self.target.uuid.bytes
        if self.label is not None:
            proto_edge.label.CopyFrom(self.label._to_protobuf())
        return proto_edge

    def __eq__(self, other):
        # type: (typing.Any) -> bool
        if not isinstance(other, Edge):
            return False
        return (
            self.source.uuid == other.source.uuid
            and self.target.uuid == other.target.uuid
            and self.label == other.label
        )

    def deep_eq(self, other):
        # type: (typing.Any) -> bool
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, Edge):
            return False
        return (
            self.source.deep_eq(other.source)
            and self.target.deep_eq(other.target)
            and self.label == other.label
        )

    def __hash__(self):
        # type: () -> int
        return hash((self.source.uuid, self.target.uuid, self.label))

    def __repr__(self):
        # type: () -> str
        return (
            "Edge("
            "source={source!r}, "
            "target={target!r}, "
            "label={label!r}, "
            ")".format(**self.__dict__)
        )
