IR
==========

**IR** describes the *internal representation* of a software artifact.

The corresponding Protobuf message type is `IR`.


Guaranteed Properties
---------------------

- An **IR** is a [Node](Node.md).

- Each **IR** object must be an [AuxDataContainer](AuxDataContainer.md).

- There must be functionality to save an **IR** object to a file, and
  to load an **IR** object from a file. This functionality is allowed
  to fail if the object's **version** does not match the [GTIRB
  Protobuf version for the API as a whole](Version.md).

- Each **IR** object must have the following information, and the API
  must provide functionality for getting and setting each.
  - The applicable Protobuf **version**.
    The default value for new **IR** objects should match the
    [GTIRB Protobuf version for the API as a whole](Version.md).
  - The **modules** in the representation, as a collection of
    [Module](Module.md) objects. The collection order will depend on
    the API language.
  - The **control flow graph** [(CFG)](CFG.md).


API Implementations
--------------------

The guaranteed functionality is provided as follows.

### IR Classes and AuxDataContainer functionality

| Language    | IR Class  | [AuxDataContainer](AuxDataContainer.md) functionality              |
|:------------|:----------|:-------------------------------------------------------------------|
| C++         | gtirb::IR | by inheritance: gtirb::IR is a subclass of gtirb::AuxDataContainer |
| Python      | gtirb.IR  | by inheritance: gtirb.IR is a subclass of gtirb.AuxDataContainer   |
| Common Lisp | **gtirb** | via **aux-data** class and specializations **aux-data** (*object* *gtirb*) => *result*, (setf (**aux-data** (*object* *gtirb*)) *new-value*) |


### GTIRB Protobuf API Version


| Language    | Get Version             | Set Version             |
|:------------|:------------------------|:------------------------|
| C++         | gtirb::IR::getVersion() | gtirb::IR::setVersion() |
| Python      | read gtirb.IR.version   | write gtirb.IR.version  |
| Common Lisp | **version** (*obj* *gtirb*) => *result* | (setf (**version** (*obj* *gtirb*)) *new*) |


### File Save/Load

| Language    | Save IR to file         | Load IR from file            |
|:------------|:------------------------|:------------------------|
| C++         | gtirb::IR::save(), gtirb::IR::saveJSON() | gtirb::IR::load(), gtirb::IR::loadJSON() |
| Python      | gtirb.IR.save_protobuf(), gtirb.IR.save_protobuf_file() | gtirb.IR.load_protobuf(), gtirb.IR.load_protobuf_file() |
| Common Lisp |  **write-gtirb** *gtirb* *path* => *result* | **read-gtirb** *path* => *result* |




### Get and Set Modules

| Language    | Get Modules                          | Set Modules            |
|-------------|:-------------------------------------|:-----------------------|
| C++         | gtirb::IR::modules()                 | gtirb::IR:: gtirb::IR::addModule(), gtirb::IR::removeModule() |
| Python      | read gtirb.IR.modules                | write gtirb.IR.modules |
| Common Lisp | **modules** (*object* *gtirb*) => result | (setf (**modules** (*object* *gtirb*)) *new-value*) |






### CFG


| Language    | Get CFG                                | Set CFG                                         |
|:------------|:---------------------------------------|:------------------------------------------------|
| C++         | gtirb::IR::getCFG()                    | ???                                             |
| Python      | read gtirb.IR.cfg                      | write gtirb.IR.cfg                              |
| Common Lisp | **cfg** (*object* *gtirb*) => *result* | (setf (**cfg** (*object* *gtirb*)) *new-value*) |


Links
--------------------

- [GTIRB Components](COMPONENTS.md)
- [Using Serialized GTIRB Data](../../PROTOBUF.md)
