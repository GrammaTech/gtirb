Symbol
====================

A **Symbol** maps a name to an object in the [IR](IR.md).

The corresponding Protobuf message type is `Symbol`.


Guaranteed Properties
---------------------

- A **Symbol** is a [Node](Node.md).

- Each **Section** object must have the following information, and the API
  must provide functionality for getting and setting each.
  - **name**
  - **optional_payload**: can be a [Block](Block.md) (sometimes called
    the *referent*) or an integer or null value (sometimes called the
    *value*)

- Each **Symbol** must belong to either zero (0) or one (1)
  [Module](Module.md) objects. The owning **Module** must be stored as
  a reference.
  - This reference may be null, in which case the **ProxyBlock** is
    freestanding and does not belong to any **Module**.
  - The reference must be readable.
  - If the reference is writable, setting it must automatically update
    the **symbols** sets for the affected **Module** object or objects
    (there will be at most two).


API Implementations
--------------------

The guaranteed functionality is provided as follows.

### Symbol Classes

| Language    | Symbol Class  |
|:------------|:--------------|
| C++         | gtirb::Symbol |
| Python      | gtirb.Symbol  |
| Common Lisp | **symbol**    |




### Required Field Getters/Setters

#### name

| Language    | Get name                 | Set name                 |
|:------------|:-------------------------|:-------------------------|
| C++         | gtirb::Symbol::getName() | gtirb::Symbol::setName() |
| Python      | read gtirb.Symbol.name   | write gtirb.Symbol.name  |
| Common Lisp | **name** (*obj* *symbol*) => *result* | (setf (**name** (*obj* *symbol*)) *new*) |


#### optional_payload (referent/value)

| Language    | Get optional_payload     | Set optional_payload     |
|:------------|:-------------------------|:-------------------------|
| C++         | gtirb::Symbol::getAddress, gtirb::Symbol::getReferent() | gtirb::Symbol::setAddress(), gtirb::Symbol::setReferent() |
| Python      | read gtirb.Symbol.referent or gtirb.Symbol.value | write gtirb.Symbol.referent or gtirb.Symbol.value |
| Common Lisp | **payload** *symbol* => *result* | (setf (**payload** *symbol*) *new*) |


### Associated Module


| Language    | Associated Module                          |
|:------------|:-------------------------------------------|
| C++         | gtirb::Symbol::getModule()                 |
| Python      | gtirb.Symbol.module                        |
| Common Lisp | **module** (*object* *symbol*) => *result* |


Links
--------------------

- [GTIRB Components](COMPONENTS.md)
- [Using Serialized GTIRB Data](../../PROTOBUF.md)
