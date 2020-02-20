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
| Common Lisp | ???                 |



### Required Field Getters/Setters

#### offset

| Language    | Get/Set offset                    |
|:------------|:----------------------------------|
| C++         | read gtirb::SymAddrConst::Offset |
| Python      | read gtirb.SymAddrConst.offset   |
| Common Lisp | ???                               |




#### symbol_uuid

| Language    | Get/Set symbol_uuid                   |
|:------------|:--------------------------------------|
| C++         | read/write gtirb::SymAddrConst::Sym  |
| Python      | read/write gtirb.SymAddrConst.symbol |
| Common Lisp | ???                                   |
