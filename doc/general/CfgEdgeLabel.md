CfgEdgeLabel
====================

A **CfgEdgeLabel** represents the optional label on an edge
([CfgEdge](CfgEdge.md)) in the interprocedural [control flow
graph](CFG.md) (CFG).

The corresponding Protobuf message type is `EdgeLabel`.



Guaranteed Properties
---------------------

- Each **CfgEdgeLabel** object must have the following information,
  and the API must provide functionality for getting and setting each.
  - **conditional**
  - **direct**
  - **type**



API Implementations
--------------------

| Language    | CfgEdgeLabel Implementation             |
|:------------|:----------------------------------------|
| C++         | gtirb::EdgeLabel                        |
| Python      | gtirb.Label (inner class of gtirb.Edge) |
| Common Lisp | **edge-label**                          |





### Required Field Getters/Setters


#### conditional

| Language    | Get/Set conditional | Get/Set conditional |
|:------------|:--------------------|:--------------------|
| C++         | read std::get<ConditionalEdge>(E) for gtirb::EdgeLabel E | read std::get<ConditionalEdge>(E) for gtirb::EdgeLabel E |
| Python      | read gtirb.CFG.label.conditional | write gtirb.CFG.label.conditional |
| Common Lisp | **conditional** (*obj* *edge-label*) => *result* | (setf (**conditional** (*obj* *edge-label*)) *new*) |


#### direct

| Language    | Get direct | Set direct |
|:------------|:-----------|:-----------|
| C++         | read std::get<DirectEdge>(E) for gtirb::EdgeLabel E | read std::get<DirectEdge>(E) for gtirb::EdgeLabel E |
| Python      | read gtirb.CFG.label.direct | write gtirb.CFG.label.direct |
| Common Lisp | **direct** (*obj* *edge-label*) => *result* | (setf (**direct** (*obj* *edge-label*)) *new*) |




#### type

| Language    | Get type | Set type |
|:------------|:---------|:---------|
| C++         |  read std::get<EdgeType>(E) for gtirb::EdgeLabel E | read std::get<EdgeType>(E) for gtirb::EdgeLabel E |
| Python      | read gtirb.CFG.label.type | write gtirb.CFG.label.type |
| Common Lisp | **edge-type** (*obj* *edge-label*) => *result* | (setf (**edge-type** (*obj* *edge-label*)) *new*) |
