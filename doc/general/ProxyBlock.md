ProxyBlock
====================

A **ProxyBlock** represents a basic block in the binary.

The corresponding Protobuf message type is `ProxyBlock`.


Guaranteed Properties
---------------------

- A **ProxyBlock** is a [CfgNode](CfgNode.md).


- Each **ProxyBlock** must belong to either zero (0) or one (1)
  [Module](Module.md) objects. The owning **Module** must be stored as
  a reference.
  - This reference may be null, in which case the **ProxyBlock** is
    freestanding and does not belong to any **Module**.
  - The reference must be readable.
  - If the reference is writable, setting it must automatically update
    the **proxy_blocks** sets for the affected **Module** object or objects
    (there will be at most two).


API Implementations
--------------------

The guaranteed functionality is provided as follows.

### ProxyBlock Classes

| Language    | ProxyBlock Class  |
|:------------|:------------------|
| C++         | gtirb::ProxyBlock |
| Python      | gtirb.ProxyBlock  |
| Common Lisp | **proxy-block**   |



### Associated Module


| Language    | Associated Module                               |
|:------------|:------------------------------------------------|
| C++         | gtirb::ProxyBlock::getModule()                  |
| Python      | gtirb.ProxyBlock.module                         |
| Common Lisp | **module** (*object* *proxy-block*) => *result* |
