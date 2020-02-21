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

| Language    | Get offset              | Set offset               |
|:------------|:------------------------|:-------------------------|
| C++         | read gtirb::SymStackConst::Offset | write gtirb::SymStackConst::Offset |
| Python      | read gtirb.SymStackConst.offset | write gtirb.SymStackConst.offset |
| Common Lisp | **offset** (*obj* *sym-stack-const*) => *result* | (setf (**offset** (*obj* *sym-stack-const*)) *new*) |




#### symbol_uuid

| Language    | Get/Set symbol_uuid  | Get/Set symbol_uuid   |
|:------------|:---------------------|:----------------------|
| C++         | read gtirb::SymStackConst::Sym  | write gtirb::SymStackConst::Sym  |
| Python      | read gtirb.SymStackConst.symbol | write gtirb.SymStackConst.symbol |
| Common Lisp | **symbols** (*object* *symbolic-expression*) => *result* | (setf (**symbols** (*object* *symbolic-expression*)) *new-value*) |


Links
--------------------

- [GTIRB Components](COMPONENTS.md)
- [Using Serialized GTIRB Data](../../PROTOBUF.md)
