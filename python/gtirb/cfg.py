import typing
from enum import Enum
from uuid import UUID

import networkx

from .block import CfgNode
from .proto import CFG_pb2


class EdgeType(Enum):
    """The type of control flow transfer indicated by an edge."""

    Branch = CFG_pb2.EdgeType.Value("Type_Branch")
    """This edge is the explicit target of a jump instruction.
    May be conditional or unconditional. If conditional, there will be
    a corresponding edge of type :attr:`gtirb.EdgeType.Fallthrough`.
    """

    Call = CFG_pb2.EdgeType.Value("Type_Call")
    """This edge is the explicit target of a call instruction.
    Unless the function does not return, there will also be a
    corresponding edge of type :attr:`gtirb.EdgeType.Fallthrough`.
    """

    Fallthrough = CFG_pb2.EdgeType.Value("Type_Fallthrough")
    """This edge represents two blocks executing in sequence.
    This occurs on the non-branching paths of conditional branch
    instructions, after call instructons have returned, and when two
    blocks have no control flow between them, but another edge targets the
    target block. If there exists a fallthrough edge from block ``A`` to
    block ``B``, then ``A`` must immediately precede ``B`` in memory.
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
    corresponding edge of type :attr:`gtirb.EdgeType.Fallthrough`. This
    is the system call equivalent to :class:`gtirb.EdgeType.Call`.
    """

    Sysret = CFG_pb2.EdgeType.Value("Type_Sysret")
    """This edge represents a return from a system call, generally via a
    return instruction. Return edges may be omitted from valid CFGs;
    a function may have an uncomputable number of possible return sites,
    due to the possibility of indirect calls. This is the system call
    equivalent to :class:`gtirb.EdgeType.Return`.
    """


class CFG(networkx.MultiDiGraph):
    """A control-flow graph for an :class:`IR`. Vertices are
    :class:`CfgNode`\\s, and edges may optionally contain edge labels.

    Edges in the graph have three optional values in thier attribute map:

    * ``type``: an :class:`EdgeType`. The type of control flow the edge
      represents.

    * ``conditional``: a ``bool``. When this edge is part of a conditional
      branch, ``conditional`` is ``True`` when the edge represents the
      control flow taken when the branch's condition is met, and
      ``False`` when it represents the control flow taken when the
      branch's condition is not met. Otherwise, it is always ``False``.

    * ``direct``: a ``bool``. ``True`` if the branch or call is direct,
      and ``False`` if it is indirect. If an edge is indirect,
      then all outgoing indirect edges represent the set of
      possible locations the edge may branch to. If there
      exists an indirect outgoing edge to a :class:`gtirb.ProxyBlock`
      without any :class:`gtirb.Symbol` objects referring to it,
      then the set of all possible branch locations is unknown.

    For efficency, only vertices with edges are guaranteed to be stored in this
    graph. If you want to find all vertices possible (that is, all
    :class:`CfgNode`\\s), use :meth:`IR.cfg_nodes` instead.
    """

    @classmethod
    def _from_protobuf(cls, edges, ir):
        # type: (typing.Iterable[CFG_pb2.Edge]) -> CFG
        result = CFG()
        for edge in edges:
            source = ir.get_by_uuid(UUID(bytes=edge.source_uuid))
            target = ir.get_by_uuid(UUID(bytes=edge.target_uuid))
            label = {}
            if edge.label is not None:
                label["type"] = EdgeType(edge.label.type)
                label["conditional"] = edge.label.conditional
                label["direct"] = edge.label.direct
            result.add_edge(source, target, **label)
        return result

    def _to_protobuf(self):
        # type: () -> typing.Iterable[CFG_pb2.Edge]
        for source, target, label in self.edges(data=True):
            proto_edge = CFG_pb2.Edge()
            proto_edge.source_uuid = source.uuid.bytes
            proto_edge.target_uuid = target.uuid.bytes
            if label:
                proto_edge.label.type = label["type"].value
                proto_edge.label.conditional = label["conditional"]
                proto_edge.label.direct = label["direct"]
            yield proto_edge

    def deep_eq(self, other):
        # type: (typing.Any) -> bool
        # Do not move __eq__. See docstring for Node.deep_eq for more info.

        def edge_sort_key(edge):
            return (
                edge[0].uuid,
                edge[1].uuid,
                sorted(edge[2].items()) if edge[2] else None,
            )

        if not isinstance(other, CFG):
            return False

        # We don't have to compare nodes for deep_eq because if an node has no
        # edges, then we do not guarantee that graphs have that node as a
        # vertex, and if it has edges, a failure of deep_eq will be detected
        # when comparing edges.

        if self.number_of_edges() != other.number_of_edges():
            return False

        self_edges = sorted(self.edges(data=True), key=edge_sort_key)
        other_edges = sorted(other.edges(data=True), key=edge_sort_key)

        for self_edge, other_edge in zip(self_edges, other_edges):
            self_source, self_target, self_attrs = self_edge
            other_source, other_target, other_attrs = other_edge
            if not self_source.deep_eq(other_source):
                return False
            if not self_target.deep_eq(other_target):
                return False
            if self_attrs != other_attrs:
                return False

        return True

    def __repr__(self):
        # type: () -> str
        return "CFG(%r)" % list(self.edges(data=True))


Edge = typing.Tuple[CfgNode, CfgNode, dict]
"""
Edges in the CFG are represented as (source, target, label) tuples. See
:class:`gtirb.CFG` for details.
"""
