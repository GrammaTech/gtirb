from enum import Enum
from uuid import UUID

import CFG_pb2

from gtirb.block import Block, ProxyBlock


class CFG:
    """
    Control Flow Graphs (CFGs)
    Interprocedural control flow graph, with vertices of type
    Block.
    """

    def __init__(self, edges=None, module=None):
        if edges is None:
            edges = set()
        self.edges = edges
        self.module = module

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = CFG_pb2.CFG()

        if self.module is not None:
            ret.vertices.extend(v.uuid.bytes for v in self.module.blocks)
            ret.vertices.extend(v.uuid.bytes for v in self.module.proxies)
        ret.edges.extend(e._to_protobuf() for e in self.edges)
        return ret

    @classmethod
    def _from_protobuf(cls, cfg, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        return cls({Edge._from_protobuf(e, uuid_cache) for e in cfg.edges})

    def add_vertex(self, vertex):
        """Add a Block/ProxyBlock vertex to CFG.

        :param vertex: the Block/ProxyBlock

        """
        if isinstance(vertex, Block):
            self.module.blocks.add(vertex)
        elif isinstance(vertex, ProxyBlock):
            self.module.proxies.add(vertex)

    def add_edge(self, edge):
        """ Add an Edge to the CFG """
        self.edges.add(edge)
        self.add_vertex(edge.source)
        self.add_vertex(edge.target)

    def remove_edges(self, edges):
        """ Remove a set of edges from the CFG """
        self.edges -= edges


class Edge:
    """
    An Edge in the CFG. Consists of a source and target Block
    """

    def __init__(self, label, source, target):
        self.label = label
        self.source = source
        self.target = target

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = CFG_pb2.Edge()
        ret.source_uuid = self.source.uuid.bytes
        ret.target_uuid = self.target.uuid.bytes
        ret.label.CopyFrom(self.label._to_protobuf())
        return ret

    @classmethod
    def _from_protobuf(cls, edge, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        source_uuid = UUID(bytes=edge.source_uuid)
        target_uuid = UUID(bytes=edge.target_uuid)
        source = None
        target = None
        if uuid_cache is not None:
            source = uuid_cache.get(source_uuid)
            target = uuid_cache.get(target_uuid)
        return cls(EdgeLabel._from_protobuf(edge.label), source, target)


class EdgeLabel:
    """
    A label on a CFG edge.
    """
    class EdgeType(Enum):
        """
        Indicates the type of control flow transfer indicated by this
        edge.
        """
        Branch = CFG_pb2.EdgeType.Value('Type_Branch')
        Call = CFG_pb2.EdgeType.Value('Type_Call')
        Fallthrough = CFG_pb2.EdgeType.Value('Type_Fallthrough')
        Return = CFG_pb2.EdgeType.Value('Type_Return')
        Syscall = CFG_pb2.EdgeType.Value('Type_Syscall')
        Sysret = CFG_pb2.EdgeType.Value('Type_Sysret')

    def __init__(self, conditional, direct, type):
        self.conditional = conditional
        self.direct = direct
        self.type = type

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = CFG_pb2.EdgeLabel()
        ret.conditional = self.conditional
        ret.direct = self.direct
        ret.type = self.type.value
        return ret

    @classmethod
    def _from_protobuf(cls, edge_label, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        return cls(edge_label.conditional, edge_label.direct,
                   EdgeLabel.EdgeType(edge_label.type))
