Block
============

**Block** represents a base class for blocks. [Symbol](Symbol.md)
objects may have references to any kind of **Block**.

Guaranteed Properties
---------------------

- A **Block** is a [Node](Node.md).


API Implementations
--------------------

The guaranteed functionality is provided as follows.

### Block Classes

| Language    | Block Class     |
|:------------|:----------------|
| C++         | no explicit **Block** class; see individual classes gtirb::CodeBlock ([CodeBlock](CodeBlock.md)), gtirb::DataBlock ([DataBlock](DataBlock.md)),  gtirb::CfgNode ([CfgNode](CfgNode.md)) |
| Python      | gtirb.Block     |
| Common Lisp | **gtirb-block** |
