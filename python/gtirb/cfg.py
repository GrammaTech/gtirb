from enum import Enum
from typing import (
    TYPE_CHECKING,
    Hashable,
    Iterable,
    Iterator,
    MutableSet,
    NamedTuple,
    Optional,
    Tuple,
)
from uuid import UUID

from networkx import MultiDiGraph

from .block import CfgNode
from .proto import CFG_pb2
from .util import DeserializationError

if TYPE_CHECKING:  # pragma: no cover
    from .ir import IR


class EdgeType(Enum):
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
    return instruction. Return edges may either go to a symbolless
    :class:`gtirb.ProxyBlock`, which indicates that the set of possible
    return targets is unknown, or there may be one return edge per
    return target, which indicates that the set of possible return targets
    if fully known.
    """

    Syscall = CFG_pb2.EdgeType.Value("Type_Syscall")
    """This edge is the explicit target of a system call instruction.
    Unless the function does not return, there will also be a
    corresponding edge of type :attr:`gtirb.Edge.Type.Fallthrough`. This
    is the system call equivalent to :class:`gtirb.Edge.Type.Call`.
    """

    Sysret = CFG_pb2.EdgeType.Value("Type_Sysret")
    """This edge represents a return from a system call, generally via a
    return instruction. Return edges may either go to a symbolless
    :class:`gtirb.ProxyBlock`, which indicates that the set of possible
    return targets is unknown, or there may be one return edge per
    return target, which indicates that the set of possible return targets
    if fully known. This is the system call equivalent to
    :class:`gtirb.Edge.Type.Return`.
    """


class EdgeLabel(NamedTuple):
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

    type: EdgeType
    conditional: bool = False
    direct: bool = True

    def __repr__(self) -> str:
        return (
            "Edge.Label("
            "type=Edge.Type.{type.name}, "
            "conditional={conditional!r}, "
            "direct={direct!r}, "
            ")".format(**self._asdict())
        )


class Edge(
    NamedTuple(
        "NamedTuple",
        (
            ("source", CfgNode),
            ("target", CfgNode),
            ("label", Optional[EdgeLabel]),
        ),
    )
):
    """An edge in the CFG from ``source`` to ``target``, with optional
    control-flow details in ``label``.

    :ivar ~.source: The source CFG node.
    :ivar ~.target: The target CFG node.
    :ivar ~.label: An optional label containing more control flow information.
    """

    __slots__ = ()

    def __new__(
        cls,
        source: CfgNode,
        target: CfgNode,
        label: Optional[EdgeLabel] = None,
    ) -> "Edge":
        return super().__new__(cls, source, target, label)

    Type = EdgeType
    Label = EdgeLabel


class CFG(MutableSet[Edge]):
    """A control-flow graph for an :class:`IR`. Vertices are
    :class:`CfgNode`\\s, and edges may optionally contain
    :class:`Edge.Label`\\s.

    The graph may be viewed simply as a set of :class:`Edge`\\s. For
    convenience, the :meth:`out_edges` and :meth:`in_edges` methods provide
    access to the outgoing or incoming edges of individual nodes.

    For efficency, only vertices with edges are guaranteed to be stored in this
    graph. If you want to find all vertices possible (that is, all
    :class:`CfgNode`\\s), use :meth:`IR.cfg_nodes` instead.

    Internally, the graph is stored as a NetworkX instance, which can be
    accessed using :meth:`nx`. This allows NetworkX's large library of graph
    algorithms to be used on CFGs, if desired.
    """

    def __init__(self, edges: Optional[Iterable[Edge]] = None):
        self._nxg: "MultiDiGraph[CfgNode, Hashable, Optional[EdgeLabel]]" = (
            MultiDiGraph()
        )
        if edges is not None:
            self.update(edges)

    def _edge_key(self, edge: Edge) -> Optional[Hashable]:
        if edge.source in self._nxg:
            neighbors = self._nxg[edge.source]
            if edge.target in neighbors:
                for key, e in neighbors[edge.target].items():
                    if "label" in e and e["label"] == edge.label:
                        return key
        return None

    def __contains__(self, edge: object) -> bool:
        return isinstance(edge, Edge) and self._edge_key(edge) is not None

    def __iter__(self) -> Iterator[Edge]:
        for s, t, l in self._nxg.edges(data="label"):
            yield Edge(s, t, l)

    def __len__(self) -> int:
        return len(self._nxg.edges())

    def update(self, edges: Iterable[Edge]) -> None:
        for edge in edges:
            self.add(edge)

    def add(self, edge: Edge) -> None:
        if edge not in self:
            self._nxg.add_edge(edge.source, edge.target, label=edge.label)

    def clear(self) -> None:
        self._nxg.clear()

    def discard(self, edge: Edge) -> None:
        key = self._edge_key(edge)
        if key is not None:
            self._nxg.remove_edge(edge.source, edge.target, key=key)

    def out_edges(self, node: CfgNode) -> Iterator[Edge]:
        if node in self._nxg:
            for s, t, l in self._nxg.out_edges(node, data="label"):
                yield Edge(s, t, l)

    def in_edges(self, node: CfgNode) -> Iterator[Edge]:
        if node in self._nxg:
            for s, t, l in self._nxg.in_edges(node, data="label"):
                yield Edge(s, t, l)

    @classmethod
    def _from_protobuf(
        cls, edges: Iterable[CFG_pb2.Edge], ir: Optional["IR"]
    ) -> "CFG":
        assert ir

        def make_edge(ir: "IR", edge: CFG_pb2.Edge) -> Edge:
            source_uuid = UUID(bytes=edge.source_uuid)
            source = ir.get_by_uuid(source_uuid)
            if not isinstance(source, CfgNode):
                raise DeserializationError(
                    "CFG: UUID %s is not a CfgNode" % source_uuid
                )

            target_uuid = UUID(bytes=edge.target_uuid)
            target = ir.get_by_uuid(target_uuid)
            if not isinstance(target, CfgNode):
                raise DeserializationError(
                    "CFG: UUID %s is not a CfgNode" % target_uuid
                )

            label: Optional[EdgeLabel] = None
            if edge.HasField("label"):
                label = Edge.Label(
                    Edge.Type(edge.label.type),
                    edge.label.conditional,
                    edge.label.direct,
                )

            return Edge(source, target, label)

        return CFG(make_edge(ir, edge) for edge in edges)

    def _to_protobuf(self) -> Iterable[CFG_pb2.Edge]:
        for s, t, l in self._nxg.edges(data="label"):
            proto_edge = CFG_pb2.Edge()
            proto_edge.source_uuid = s.uuid.bytes
            proto_edge.target_uuid = t.uuid.bytes
            if l:
                proto_edge.label.type = l.type.value
                proto_edge.label.conditional = l.conditional
                proto_edge.label.direct = l.direct
            yield proto_edge

    # Note: This returns a "bare" MultiDiGraph because MultiDiGraph is not
    # actually a generic type.
    def nx(self) -> MultiDiGraph:  # type: ignore[type-arg]
        return self._nxg

    def deep_eq(self, other: "CFG") -> bool:
        # Do not move __eq__. See docstring for Node.deep_eq for more info.

        def edge_sort_key(
            edge: Edge,
        ) -> Tuple[UUID, UUID, Optional[Tuple[int, bool, bool]]]:
            label_key = -1, False, False
            if edge.label is not None:
                label_key = (
                    edge.label.type.value,
                    edge.label.conditional,
                    edge.label.direct,
                )
            return (edge.source.uuid, edge.target.uuid, label_key)

        if not isinstance(other, CFG):
            return False

        # We don't have to compare nodes for deep_eq because if an node has no
        # edges, then we do not guarantee that graphs have that node as a
        # vertex, and if it has edges, a failure of deep_eq will be detected
        # when comparing edges.

        if self._nxg.number_of_edges() != other._nxg.number_of_edges():
            return False

        self_edges = sorted(self, key=edge_sort_key)
        other_edges = sorted(other, key=edge_sort_key)

        for self_edge, other_edge in zip(self_edges, other_edges):
            if self_edge.label != other_edge.label:
                return False
            if not self_edge.source.deep_eq(other_edge.source):
                return False
            if not self_edge.target.deep_eq(other_edge.target):
                return False

        return True

    def __repr__(self) -> str:
        return "CFG(%r)" % list(self)
