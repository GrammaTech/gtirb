SymStackConst
====================

A **SymStackConst** represents a symbolic operand of the form "Sym +
Offset", representing an offset from a stack variable.

The corresponding Protobuf message type is `SymStackConst`.


Guaranteed Properties
---------------------

- **SymStackConst** is one of the permissible types for a
  [SymbolicExpression](SymbolicExpression.md).


- Each **SymStackConst** object must have the following information,
  and the API must provide functionality for getting and setting each.
  - **offset**
  - **symbol_uuid** (as a [Symbol](Symbol.md))



API Implementations
--------------------

The guaranteed functionality is provided as follows.

### SymStackConst Classes

| Language    | SymStackConst Class  |
|:------------|:---------------------|
| C++         | gtirb::SymStackConst |
| Python      | gtirb.SymStackConst  |
| Common Lisp | **sym-stack-const**  |



### Required Field Getters/Setters

#### offset

| Language    | Get/Set offset                    |
|:------------|:----------------------------------|
| C++         | read gtirb::SymStackConst::Offset |
| Python      | read gtirb.SymStackConst.offset   |
| Common Lisp | ???                               |




#### symbol_uuid

| Language    | Get/Set symbol_uuid                   |
|:------------|:--------------------------------------|
| C++         | read/write gtirb::SymStackConst::Sym  |
| Python      | read/write gtirb.SymStackConst.symbol |
| Common Lisp | ???                                   |


Links
--------------------

- [GTIRB Components](COMPONENTS.md)
- [Using Serialized GTIRB Data](../../PROTOBUF.md)
