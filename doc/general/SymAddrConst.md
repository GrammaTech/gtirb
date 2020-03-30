SymAddrConst
====================

A **SymAddrConst** represents a symbolic operand of the form "Sym +
Offset".

The corresponding Protobuf message type is `SymAddrConst`.


Guaranteed Properties
---------------------

- **SymAddrConst** is one of the permissible types for a
  [SymbolicExpression](SymbolicExpression.md).

- Each **SymAddrConst** object must have the following information,
  and the API must provide functionality for getting and setting each.
  - **offset**
  - **symbol_uuid** (as a [Symbol](Symbol.md))



API Implementations
--------------------

The guaranteed functionality is provided as follows.

### SymAddrConst Classes

| Language    | SymAddrConst Class  |
|:------------|:--------------------|
| C++         | gtirb::SymAddrConst |
| Python      | gtirb.SymAddrConst  |
| Common Lisp | **sym-addr-const**  |



### Required Field Getters/Setters

#### offset

| Language    | Get offset          | Set offset            |
|:------------|:--------------------|:----------------------|
| C++         | read gtirb::SymAddrConst::Offset | write gtirb::SymAddrConst::Offset |
| Python      | read gtirb.SymAddrConst.offset | write gtirb.SymAddrConst.offset |
| Common Lisp | **offset** (*obj* *sym-addr-const*) => *result* | (setf (**offset** (*obj* *sym-addr-const*)) *new*) |




#### symbol_uuid

| Language    | Get symbol_uuid   | Get/Set symbol_uuid   |
|:------------|:------------------|:----------------------|
| C++         | read gtirb::SymAddrConst::Sym  | write gtirb::SymAddrConst::Sym  |
| Python      | read gtirb.SymAddrConst.symbol | read gtirb.SymAddrConst.symbol |
| Common Lisp | **symbols** (*object* *symbolic-expression*) => *result* | (setf (**symbols** (*object* *symbolic-expression*)) *new-value*) |


Links
--------------------

- [GTIRB Components](COMPONENTS.md)
- [Using Serialized GTIRB Data](../../PROTOBUF.md)
