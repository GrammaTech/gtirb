ByteInterval
====================

A **ByteInterval** represents a named section of a binary.

The corresponding Protobuf message type is `ByteInterval`.


Guaranteed Properties
---------------------

- A **ByteInterval** is a [Node](Node.md).

- Each **ByteInterval** object must have the following information,
  and the API must provide functionality for getting and setting each.

  - **blocks** (as a collection of [ByteBlock](ByteBlock.md) objects,
    in offset order)

  - **symbolic_expressions** (as a collection of
    [SymbolicExpression](SymbolicExpression.md) objects, in offset
    order)

  - **contents** (as a byte array)

  - **size**

  - **address** (Must be either optional or nullable. It is not valid
    to have a sentinel value in the range of a 64-bit integer as an
    indication of nullness.)

- A **ByteInterval** must provide the following functionality for its
  **blocks**.
  - Insert a **ByteBlock** at a specified offset.
  - Remove an existing **ByteBlock**.
  - Retrieve all **ByteBlock** objects at a specified offset.
  - Iterate over all **ByteBlock** objects.
  - Find all **CodeBlock** objects that intersect a given address or
    range of addresses.
  - Find all **CodeBlock** objects that start at a given address or
    range of addresses. This operation must complete in O(m + log n) time,
    where m is the size of the returned set and n is the number of
    **ByteBlock** objects in **blocks**.
  - Find all **DataBlock** objects that intersect a given address or
    range of addresses.
  - Find all **DataBlock** objects that start at a given address or
    range of addresses. This operation must complete in O(m + log n) time,
    where m is the size of the returned set and n is the number of
    **ByteBlock** objects in **blocks**.

- A **ByteInterval** must provide the following functionality for its
  **symbolic_expressions**.
  - Insert a **SymbolicExpression** at a specified offset.
  - Remove an existing **SymbolicExpression**.
  - Retrieve all **SymbolicExpression** objects at a specified offset.
  - Iterate over all **SymbolicExpression** objects.
  - Find all **SymbolicExpression** objects that start at a given
    address or range of addresses. This operation must complete in
    O(log n) time, where n is the number of **SymbolicExpression**
    objects in **symbolic_expressions**. (There is no corresponding
    address *intersection* requirement: **SymbolicExpressions** have no size.)


- The **contents** have the following requirements.
  - There must be functionality for reading and writing the length of
    the **contents** byte array (the *initialized size*). Writing must
    resize the byte array, either by truncation or by zero-padding.
  - The size of **contents** must be less than or equal to the size of
    the **ByteInterval**.
  - If the size of a **ByteInterval** is changed to a value that is
    less than the size of its **contents**, its **contents** must be
    truncated.


- Each **ByteInterval** must belong to either zero (0) or one (1)
  [Section](Section.md) objects. The owning **Section** must be stored as
  a reference.
  - This reference may be null, in which case the **ByteInterval** is
    freestanding and does not belong to any **Section**.



API Implementations
--------------------

The guaranteed functionality is provided as follows.

### ByteInterval Classes

| Language    | ByteInterval Class  |
|:------------|:--------------------|
| C++         | gtirb::ByteInterval |
| Python      | gtirb.ByteInterval  |
| Common Lisp | **byte-interval**   |


### blocks

#### Get and Set


| Language    | Get blocks | Set blocks |
|:------------|:---------------|:---------------|
| C++         | gtirb::ByteInterval::blocks() | gtirb::ByteInterval::addBlock(), gtirb::ByteInterval::RemoveBlock() |
| Python      | read gtirb.ByteInterval.blocks | write gtirb.ByteInterval.blocks |
| Common Lisp | **blocks** (*object* *byte-interval*) => *result* | (setf (**blocks** (*object* *byte-interval*)) *new-value*) |



#### ByteBlock operations


| Language    | Insert at offset | Remove | Retrieve from offset | Iterate |
|:------------|:---------------|:---------------|:---------------|:---------------|
| C++         | gtirb::ByteInterval::addBlock() | gtirb::ByteInterval::removeBlock() | gtirb::ByteInterval::findBlocksAtOffset | gtirb::ByteInterval::blocks_begin() |
| Python      | for all these operations, interact directly with gtirb.ByteInterval.contents: each gtirb.ByteBlock object stores its own offset. | . | . | . |
| Common Lisp | for all these operations, interact with the contents through the **contents** accessor: each **byte-block** object stores its own offset. | . | . | . |


#### Find  CodeBlock Objects...

| Language    | ...that intersect an address/range | ...that begin at an address/range
|:------------|:---------------------------|:---------------------------|
| C++         | gtirb::ByteInterval::findCodeBlocksOn() | gtirb::ByteInterval::findCodeBlocksAt() |
| Python      | gtirb.ByteInterval.code_blocks_in() | gtirb.ByteInterval.code_blocks_at() |
| Common Lisp | **in-address** *object* *start-address* &optional *end-address* => *result*, then filter *result* to extract the **code-block** objects | **at-address** *object* *address* => *result*, then filter *result* to extract the **code-block** objects. [*] |

[*] Address range checking is not yet implemented for Common Lisp
**at-address**


#### Find  DataBlock Objects...

| Language    | ...that intersect an address/range | ...that begin at an address/range
|:------------|:---------------------------|:---------------------------|
| C++         | gtirb::ByteInterval::findDataBlocksOn() | gtirb::ByteInterval::findDataBlocksAt() |
| Python      | gtirb.ByteInterval.data_blocks_in() | gtirb.ByteInterval.data_blocks_at() |
| Common Lisp |  **in-address** *object* *start-address* &optional *end-address* => *result*, then filter *result* to extract the **data-block** objects | **at-address** *object* *address* => *result*, then filter *result* to extract the **data-block** objects. [*] |

[*] Address range checking is not yet implemented for Common Lisp
**at-address**


### symbolic_expressions

#### Get and Set

| Language    | Get symbolic_expressions | Set symbolic_expressions |
|:------------|:---------------|:---------------|
| C++         | gtirb::ByteInterval | gtirb::ByteInterval |
| Python      | gtirb.ByteInterval |  gtirb.ByteInterval |
| Common Lisp | **symbolic-expressions** (*object* *byte-interval*) => *result* | (setf (**symbolic-expressions** (*object* *byte-interval*)) *new-value*) |


#### Find SymbolicExpression Objects...

| Language    | ...that begin at an address/range                |
|:------------|:-------------------------------------------------|
| C++         | gtirb::ByteInterval::findSymbolicExpressionsAt() |
| Python      | gtirb.ByteInterval.symbolic_expressions_at()     |
| Common Lisp |  **at-address** *object* *address* => *result*, then filter *result* to extract the symbolic expression objects. [*] |

[*] Address range checking is not yet implemented for Common Lisp
**at-address**

#### Other Required SymbolicExpression Operations


| Language    | Insert at offset | Remove | Retrieve from offset | Iterate |
|:------------|:---------------|:---------------|:---------------|:---------------|
| C++         | gtirb::ByteInterval::addSymbolicExpression() | gtirb::ByteInterval::removeBlock() | gtirb::ByteInterval::findSymbolicExpressionsAtOffset | gtirb::ByteInterval::symbolic_expressions_begin() |
| Python      | for all these operations, interact directly with gtirb.ByteInterval.symbolic_expressions, which is a mapping indexed by offset. | . | . | . |
| Common Lisp | for all these operations, interact with the SymbolicExpression objects through the **symbolic-expressions** accessor: **symbolic-expression** hashes are keyed by offset. | . | . | . |



### contents

#### Get and Set


| Language    | Get contents | Set contents |
|:------------|:---------------|:---------------|
| C++         | gtirb::ByteInterval | gtirb::ByteInterval |
| Python      | gtirb.ByteInterval |  gtirb.ByteInterval |
| Common Lisp | **contents** (*obj* *byte-interval*) => *result* | (setf (**contents** (*obj* *byte-interval*)) *new*) |


#### Initialized Size


| Language    | Get initialized size | Set initialized size |
|:------------|:---------------------|:---------------------|
| C++         | gtirb::ByteInterval::getInitializedSize() | gtirb::ByteInterval::setInitializedSize() |
| Python      | get gtirb.ByteInterval.initialized_size | set gtirb.ByteInterval.initialized_size |
| Common Lisp | Use the **contents** accessor and take the length of the result | ??? |



### size

#### Get and Set


| Language    | Get size                      | Set size                      |
|:------------|:------------------------------|:------------------------------|
| C++         | gtirb::ByteInterval.getSize() | gtirb::ByteInterval.setSize() |
| Python      | read gtirb.ByteInterval.size  | write gtirb.ByteInterval.size |
| Common Lisp | **size** (*obj* *byte-interval*) => *result* | (setf (**size** (*obj* *byte-interval*)) *new*) |



### address

#### Get and Set


| Language    | Get address | Set address |
|:------------|:------------|:------------|
| C++         | gtirb::ByteInterval::getAddress() | gtirb::ByteInterval::setAddress |
| Python      | read gtirb.ByteInterval.address | write gtirb.ByteInterval.address |
| Common Lisp | **address** (*obj* *byte-interval*) => *result* | (setf (**address** (*obj* *byte-interval*)) *new*) |






### Associated Section


| Language    | Associated Section                                 |
|:------------|:---------------------------------------------------|
| C++         | gtirb::ByteInterval::getSection()                  |
| Python      | gtirb.ByteInterval.section                         |
| Common Lisp | **section** (*object* *byte-interval*) => *result* |



Links
--------------------

- [GTIRB Components](COMPONENTS.md)
- [Using Serialized GTIRB Data](../../PROTOBUF.md)
