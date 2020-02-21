CfgNode
====================

**CfgNode** represents a base class for blocks that may appear as
vertices in the control flow graph (CFG).

Guaranteed Properties
---------------------

- A **CfgNode** is a [Node](Node.md).


API Implementations
--------------------

The guaranteed functionality is provided as follows.

### CfgNode Classes

| Language    | CfgNode Class  |
|:------------|:---------------|
| C++         | gtirb::CfgNode |
| Python      | gtirb.CfgNode  |
| Common Lisp | see [http://eschulte.github.io/graph/](http://eschulte.github.io/graph/). Nodes in the CFG contain the UUIDs of **code-block** objects which can be looked up using **get-uuid**. |



Links
--------------------

- [Advice on when to Place ICFG Edges](../CFG-Edges.md)
- [GTIRB Components](COMPONENTS.md)
- [Using Serialized GTIRB Data](../../PROTOBUF.md)
