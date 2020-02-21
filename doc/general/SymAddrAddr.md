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
|:------------|:-------------------|
| C++         | gtirb::SymAddrAddr |
| Python      | gtirb.SymAddrAddr  |
| Common Lisp | **sym-addr-addr**  |



### Required Field Getters/Setters

#### scale

| Language    | Get scale       | Set scale             |
|:------------|:----------------|:----------------------|
| C++         | read gtirb::SymAddrAddr::Scale | write gtirb::SymAddrAddr::Scale |
| Python      | read gtirb.SymAddrAddr.scale | write gtirb.SymAddrAddr.scale |
| Common Lisp | **scale** (*obj* *sym-addr-addr*) => *result* | (setf (**scale** (*obj* *sym-addr-addr*)) *new*) |



#### offset

| Language    | Get offset         | Set offset    |
|:------------|:-------------------|:--------------|
| C++         | read gtirb::SymAddrAddr::Offset | write gtirb::SymAddrAddr::Offset |
| Python      | read gtirb.SymAddrAddr.offset | write gtirb.SymAddrAddr.offset |
| Common Lisp | **offset** (*obj* *sym-addr-addr*) => *result* | (setf (**offset** (*obj* *sym-addr-addr*)) *new*) |




#### symbol1_uuid

| Language    | Get symbol1_uuid    | Set symbol1_uuid   |
|:------------|:--------------------|:-----------------------|
| C++         | read gtirb::SymAddrAddr::Sym1 | write gtirb::SymAddrAddr::Sym1 |
| Python      | read gtirb.SymAddrAddr.symbol1 | write gtirb.SymAddrAddr.symbol1 |
| Common Lisp | **symbols** (*object* *symbolic-expression*) => *result*, first **symbol** in *result* | (setf (**symbols** (*object* *symbolic-expression*)) *new-value*), first **symbol** in *new-value* |



#### symbol2_uuid


| Language    | Get symbol2_uuid     | Set symbol2_uuid    |
|:------------|:---------------------|:--------------------|
| C++         | read gtirb::SymAddrAddr::Sym2  | read/write gtirb::SymAddrAddr::Sym2  |
| Python      | read gtirb.SymAddrAddr.symbol2 | read/write gtirb.SymAddrAddr.symbol2 |
| Common Lisp | **symbols** (*object* *symbolic-expression*) => *result*, second **symbol** in *result* | (setf (**symbols** (*object* *symbolic-expression*)) *new-value*), second **symbol** in *new-value* |

Links
--------------------

- [GTIRB Components](COMPONENTS.md)
- [Using Serialized GTIRB Data](../../PROTOBUF.md)
