SymbolicExpression
====================

A **SymbolicExpression** represents a data value or instruction
operand which should be interpreted as referring to a symbol.



Guaranteed Properties
---------------------

- A **SymbolicExpression** must be one of the following, implemented
  as either an inheritance hierarchy or a tagged union.
  - [SymStackConst](SymStackConst.md)
  - [SymAddrConst](SymAddrConst.md)
  - [SymAddrAddr](SymAddrAddr.md)




API Implementations
--------------------

The guaranteed functionality is provided as follows.

| Language    | SymbolicExpression Implementation |
|:------------|:----------------------------------|
| C++         | gtirb::SymbolicExpression         |
| Python      | gtirb.SymbolicExpression          |
| Common Lisp | ???                               |
