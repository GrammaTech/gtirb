CfgEdge
====================

A **CfgEdge** represents an edge in the interprocedural [control flow
graph](CFG.md) (CFG).

The corresponding Protobuf message type is `Edge`.


- A **CfgEdge** may optionally store a [CfgEdgeLabel](CfgEdgeLabel.md).

- There are no guaranteed properties for the **CFG**. There are,
  however, guaranteed properties for constituent component
  [CfgEdgeLabel](CfgEdgeLabel.md).


API Implementations
--------------------

| Language    | CfgEdge Implementation                            |
|:------------|:--------------------------------------------------|
| C++         | no explicit type: add edges with gtirb::addEdge() |
| Python      | gtirb.Edge                                        |
| Common Lisp | see [http://eschulte.github.io/graph/](http://eschulte.github.io/graph/). Edges in the CFG are labeled with **edge-label** objects. |



Links
--------------------

- [Advice on when to Place ICFG Edges](../CFG-Edges.md)
- [GTIRB Components](COMPONENTS.md)
- [Using Serialized GTIRB Data](../../PROTOBUF.md)
