# GTIRB Node


**Node** is the root class for many [GTIRB components](ComponentIndex.md).



## Guaranteed Properties


- A unique identifier, **UUID**
   - The UUID is readable.

- A facility to look up Node objects by UUID.
   - Any node still reachable in memory must be retrievable by UUID.
   

## API Implementations

The guaranteed functionality is provided as follows.

| Language    | Node Class  | Retrieve UUID for Node N | Retrieve Node by UUID    |
|-------------|-------------|--------------------------|--------------------------|
| C++         | gtirb::Node | N.getUUID()              | gtirb::Node::getByUUID() |
| Python      | gtirb.node  | N.uuid                   | gtirb.node.from_uuid()   |
| Common Lisp | -           | (uuid N)                 | get-uuid                 |





