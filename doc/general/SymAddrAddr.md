SymAddrAddr
====================

A **SymAddrAddr** represents a symbolic operand of the form "(Sym1 -
Sym2) / Scale + Offset".

The corresponding Protobuf message type is `SymAddrAddr`.


Guaranteed Properties
---------------------

- **SymAddrAddr** is one of the permissible types for a
  [SymbolicExpression](SymbolicExpression.md).

- Each **SymAddrAddr** object must have the following information,
  and the API must provide functionality for getting and setting each.
  - **scale**
  - **offset**
  - **symbol1_uuid** (as a [Symbol](Symbol.md))
  - **symbol2_uuid** (as a [Symbol](Symbol.md))



API Implementations
--------------------

The guaranteed functionality is provided as follows.

### SymAddrAddr Classes

| Language    | SymAddrAddr Class  |
|:------------|:--------------------|
| C++         | gtirb::SymAddrAddr |
| Python      | gtirb.SymAddrAddr  |
| Common Lisp | ???                 |



### Required Field Getters/Setters

#### scale

| Language    | Get/Set scale                  |
|:------------|:-------------------------------|
| C++         | read gtirb::SymAddrAddr::Scale |
| Python      | read gtirb.SymAddrAddr.offset  |
| Common Lisp | ???                            |



#### offset

| Language    | Get/Set offset                  |
|:------------|:--------------------------------|
| C++         | read gtirb::SymAddrAddr::Offset |
| Python      | read gtirb.SymAddrAddr.offset   |
| Common Lisp | ???                             |




#### symbol1_uuid

| Language    | Get/Set symbol1_uuid                 |
|:------------|:-------------------------------------|
| C++         | read/write gtirb::SymAddrAddr::Sym1  |
| Python      | read/write gtirb.SymAddrAddr.symbol1 |
| Common Lisp | ???                                  |



#### symbol2_uuid

| Language    | Get/Set symbol2_uuid                 |
|:------------|:-------------------------------------|
| C++         | read/write gtirb::SymAddrAddr::Sym2  |
| Python      | read/write gtirb.SymAddrAddr.symbol2 |
| Common Lisp | ???                                  |
