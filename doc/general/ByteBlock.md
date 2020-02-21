ByteBlock
====================

**ByteBlock** is the base class for blocks that belong to a
[ByteInterval](ByteInterval.md) and store their bytes there.


Guaranteed Properties
---------------------

- A **ByteBlock** is a [Block](Block.md).

- Each **ByteBlock** object must store an integral **size** that is
  readable and writable.

- Each **ByteBlock** must belong to either zero (0) or one (1)
  [ByteInterval](ByteInterval.md) objects. The owning **ByteInterval**
  must be stored as a reference.
  - This reference may be null, in which case the **ByteBlock** is
    freestanding and does not belong to any **ByteInterval**.

- If a **ByteBlock** belongs to a **ByteInterval** then it must be
  able to retrieve its offset in the **ByteInterval**.

- If a **ByteBlock** belongs to a **ByteInterval** that has an address
  then it must be able to retrieve its address, calculated as the
  **ByteInterval** address plus the **ByteBlock** offset.


API Implementations
--------------------

The guaranteed functionality is provided as follows.

### ByteBlock Classes

| Language    | ByteBlock Class |
|:------------|:----------------|
| C++         | no explicit **ByteBlock** class; see individual classes gtirb::CodeBlock ([CodeBlock](CodeBlock.md)), gtirb::DataBlock ([DataBlock](DataBlock.md)) |
| Python      | gtirb.ByteBlock    |
| Common Lisp | **gtirb-byte-block** |



### Size Getters/Setters

| Language    | Get size                  | Set size                  |
|:------------|:--------------------------|:--------------------------|
| C++         | gtirb::CodeBlock::getSize(), gtirb::DataBlock::getSize() | gtirb::CodeBlock::setSize(), gtirb::DataBlock::setSize() |
| Python      | read gtirb.ByteBlock.size   | write gtirb.ByteBlock.size  |
| Common Lisp | **size** (*obj* *code-block*) => *result*, **size** (*obj* *data-block*) => *result* | (setf (**size** (*obj* *code-block*)) *new*), (setf (**size** (*obj* *data-block*)) *new*) |


### Associated ByteInterval

#### Get Associated ByteInterval

| Language    | Get Associated ByteInterval         |
|:------------|:------------------------------------|
| C++         | gtirb::ByteBlock::getByteInterval() |
| Python      | gtirb.ByteBlock.byte_interval       |
| Common Lisp | **byte-interval** (*object* *code-block*) => *result*,  **byte-interval** (*object* *data-block*) => *result* |


#### ByteBlock Offset in Associated ByteInterval

| Language    | Offset                              |
|:------------|:------------------------------------|
| C++         | gtirb::CodeBlock::getOffset(), gtirb::DataBlock::getOffset() |
| Python      | gtirb.ByteBlock.offset              |
| Common Lisp | **offset** (*obj* *code-block*) => *result*,  **offset** (*obj* *data-block*) => *result* |



#### ByteBlock Address

| Language    | Address                          |
|:------------|:---------------------------------|
| C++         | gtirb::DataBlock::getAddress(), gtirb::DataBlock::getAddress() |
| Python      | gtirb.ByteBlock.address          |
| Common Lisp | **address** *obj* => *result*    |


Links
--------------------

- [GTIRB Components](COMPONENTS.md)
- [Using Serialized GTIRB Data](../../PROTOBUF.md)
