Node
==========

**Node** is the root class for many [GTIRB components](COMPONENTS.md).


Guaranteed Properties
---------------------

- A unique identifier, **UUID**
   - The UUID is readable.

- A facility to look up Node objects by UUID.
   - Any node still reachable in memory must be retrievable by UUID.

- All APIs must implement a notion of **Node**.



API Implementations
--------------------

The guaranteed functionality is provided as follows.

| Language    | Node Class  | Retrieve UUID for Node        | Retrieve Node by UUID                    |
|-------------|-------------|-------------------------------|------------------------------------------|
| C++         | gtirb::Node | gtirb::Node::getUUID()        | gtirb::Node::getByUUID()                 |
| Python      | gtirb.Node  | gtirb.Node.uuid               | gtirb.Node.from_uuid()                   |
| Common Lisp | -           | **uuid** *object* => *result* | **get-uuid** *uuid* *object* => *result* |


Links
--------------------

- [GTIRB Components](COMPONENTS.md)
- [Using Serialized GTIRB Data](../../PROTOBUF.md)
