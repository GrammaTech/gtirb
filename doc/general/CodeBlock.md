CodeBlock
====================

A **CodeBlock** represents a basic block in the binary.

The corresponding Protobuf message type is `CodeBlock`.


Guaranteed Properties
---------------------

- A **CodeBlock** is a [ByteBlock](ByteBlock.md) and a [CfgNode](CfgNode.md).

- Each **CodeBlock** must store its **decode_mode**, and this value
  must be readable and writable.


API Implementations
--------------------

The guaranteed functionality is provided as follows.

### CodeBlock Classes

| Language    | CodeBlock Class  |
|:------------|:-----------------|
| C++         | gtirb::CodeBlock |
| Python      | gtirb.CodeBlock  |
| Common Lisp | **code-block**   |



### decode_mode Getters/Setters

| Language    | Get decode_mode           | Set decode_mode           |
|:------------|:--------------------------|:--------------------------|
| C++         | gtirb::CodeBlock::getDecodeMode() | gtirb::Section::setDecodeMode() |
| Python      | read gtirb.CodeBlock.decode_mode | write gtirb.CodeBlock.decode_mode  |
| Common Lisp | **decode-mode** (*obj* *code-block*) => *result* | (setf (**decode-mode** (*obj* *code-block*)) *new*) |


Links
--------------------

- [GTIRB Components](COMPONENTS.md)
- [Using Serialized GTIRB Data](../../PROTOBUF.md)
