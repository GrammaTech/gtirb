CFG
====================

A **CFG** represents the interprocedural control flow graph.

The corresponding Protobuf message type is `CFG`.

- The vertices of a **CFG** correspond to [CfgNode](CfgNode.md)
  - objects.  Every **CfgNode** object in the [IR](IR.md) is
    potentially a vertex in the **CFG**.

- The edges of a **CFG** correspond to [CfgEdge](CfgEdge.md) objects.

- There are no guaranteed properties for the **CFG**. There are,
  however, guaranteed properties for constituent components
  [CfgNode](CfgNode.md) and [CfgEdgeLabel](CfgEdgeLabel.md).


API Implementations
--------------------


| Language    | CFG Implementation     |
|:------------|:-----------------------|
| C++         | gtirb::CFG             |
| Python      | typing.set[gtirb.Edge] |
| Common Lisp | a [graph:digraph](http://eschulte.github.io/graph/) whose nodes hold **code-block** UUIDs and edges are labeled with **edge_label** objects    |


Links
--------------------

- [Advice on when to Place ICFG Edges](../CFG-Edges.md)
- [GTIRB Components](COMPONENTS.md)
- [Using Serialized GTIRB Data](../../PROTOBUF.md)
