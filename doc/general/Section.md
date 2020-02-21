Section
==========

A **Section** represents a named section of a binary.

The corresponding Protobuf message type is `Section`.


Guaranteed Properties
---------------------

- A **Section** is a [Node](Node.md).

- Each **Section** object must have the following information, and the API
  must provide functionality for getting and setting each.
  - **name**
  - **section_flags** (as a bitset or enumeration set)
  - **byte_intervals** (as a collection of
    [ByteInterval](ByteInterval.md), order defined by target language)

- Each **Section** must belong to either zero (0) or one (1)
  [Module](Module.md) objects. The owning **Module** must be stored as
  a reference.
  - This reference may be null, in which case the **Section** is
    freestanding and does not belong to any Module.

- The following operations must be available for the set of
  [ByteInterval](ByteInterval.md) objects in the **Section**.
  - Find all **ByteInterval** objects that intersect a given address or
    range of addresses.
  - Find all **ByteInterval** objects that begin at a given address or
    range of addresses. This operation must complete in O(M + log n) time
    where m is the size of the returned set and n is the number of
    **ByteInterval** objects in the **Section**.


API Implementations
--------------------

The guaranteed functionality is provided as follows.

### Section Classes

| Language    | Section Class  |
|:------------|:---------------|
| C++         | gtirb::Section |
| Python      | gtirb.Section  |
| Common Lisp | **section**    |



### Required Field Getters/Setters

#### name

| Language    | Get name                  | Set name                  |
|:------------|:--------------------------|:--------------------------|
| C++         | gtirb::Section::getName() | gtirb::Section::setName() |
| Python      | read gtirb.Section.name   | write gtirb.Section.name  |
| Common Lisp | **name** (*obj* *section*) => *result* | (setf (**name** (*obj* *section*)) *new*) |


#### section_flags


| Language    | Get section_flags        | Set section_flags         |
|:------------|:-------------------------|:--------------------------|
| C++         | gtirb::Section::flags()  | gtirb::Section::addFlag(), gtirb::Section::addFlags(), gtirb::Section::removeFlag() |
| Python      | read gtirb.Section.flags | write gtirb.Section.flags |
| Common Lisp | **flags** (*obj* *section*) => *result* | (setf (**flags** (*obj* *section*)) *new*)  |


#### byte_intervals


| Language    | Get byte_intervals                | Set byte_intervals |
|:------------|:----------------------------------|:-------------------|
| C++         | gtirb::Section::byte_intervals()  | gtirb::Section::addByteInterval(), gtirb::Section::removeByteInterval() |
| Python      | read gtirb.Section.byte_intervals | write gtirb.Section.byte_intervals  |
| Common Lisp | **byte-intervals** (*object* *section*) => *result* | (setf (**byte-intervals** (*object* *section*)) *new-value)* |




### Find ByteInterval Objects...

| Language    | ...that intersect an address/range | ...that begin at an address/range
|:------------|:---------------------------|:---------------------------|
| C++         | gtirb::Section::findByteIntervalsOn() | gtirb::Section::findByteIntervalsAt() |
| Python      | gtirb.Section.byte_intervals_in() | gtirb.Section.byte_intervals_at() |
| Common Lisp |  **in-address** *object* *start-address* &optional *end-address* => *result*, then filter *result* to extract the **byte-interval** objects | **at-address** *object* *address* => *result*, then filter *result* to extract the **byte-interval** objects. [*] |

[*] Address range checking is not yet implemented for Common Lisp
**at-address**




### Associated Module


| Language    | Associated Module                           |
|:------------|:--------------------------------------------|
| C++         | gtirb::Section::getModule()                 |
| Python      | gtirb.Section.module                        |
| Common Lisp | **module** (*object* *section*) => *result* |
