Module
=======

A **Module** represents a single binary (library or executable).

The corresponding Protobuf message type is `Module`.


Guaranteed Properties
---------------------

- A **Module** is a [Node](Node.md).

- Each **Module** object must be an
  [AuxDataContainer](AuxDataContainer.md).

- Each **Module** object must have the following information, and the
  API must provide functionality for getting and setting each.
  - **binary_path**
  - **preferred_addr**
  - **rebase_delta**
  - **file_format**
  - **isa**
  - **name**
  - **sections** (as a collection of [Section](Section.md), order
      defined by target language)
  - **symbols** (as a collection of [Symbol](Symbol.md), order defined
      by target language)
  - **proxy_blocks** (as a collection of [ProxyBlock](ProxyBlock.md),
      order defined by target language)
  - **entry_point** (as a [CodeBlock](CodeBlock.md), or null if empty)

- Functionality for conversion to and from a Protobuf message of type
  `Module` must be provided, and must account for the required fields
  listed above.

- Each **Module** must belong to either zero (0) or one (1)
  [IR](IR.md) objects. The owning **IR** must be stored as a
  reference.
  - This reference may be null, in which case the **Module**
    is freestanding and does not belong to any **IR**.
  - The reference must be readable.
  - If the reference is writable, setting it must automatically update
    the module sets for the affected **IR** object or objects (there
    will be at most two).

- The following operations must be available for the set of
  [Section](Section.md) objects in the **Module**.
  - Find all **Section** objects that intersect a given address or
    range of addresses.
  - Find all **Section** objects that begin at a given address or
    range of addresses. This operation must complete in O(log n) time,
    where m is the size of the returned set and n is the number of
    **Section** objects in the **Module**.



## API Implementations

The guaranteed functionality is provided as follows.


### Module Classes and AuxDataContainer functionality

| Language    | Module Class  | [AuxDataContainer](AuxDataContainer.md) functionality |
|:------------|:--------------|:-------------------------------|
| C++         | gtirb::Module | by inheritance: gtirb::Module is a subclass of gtirb::AuxDataContainer |
| Python      | gtirb.Module  | by inheritance: gtirb.Module is a subclass of gtirb.AuxDataContainer |
| Common Lisp | **module**    | via **aux-data** class and specializations **aux-data** (*object* *module*) => *result*, (setf (**aux-data** (*object* *module*)) *new-value*) |


### Required Field Getters/Setters


#### binary_path

| Language    | Get binary_path | Set binary_path |
|:------------|:----------------|:----------------|
| C++         | gtirb::Module::getBinaryPath() | gtirb::Module::setBinaryPath() |
| Python      | read gtirb.Module.binary_path  | write gtirb.Module.binary_path |
| Common Lisp | **binary-path** (*obj* *module*) => *result* | (setf (**binary-path** (*obj* *module*)) *new*) |


#### preferred_addr

| Language    | Get preferred_addr | Set preferred_addr  |
|:------------|:-------------------|:--------------------|
| C++         | gtirb::Module::getPreferredAddr() | gtirb::Module::setPreferredAddr() |
| Python      | read gtirb.Module.isa  | write gtirb.Module.isa |
| Common Lisp | **preferred-addr** (*obj* *module*) => *result* | (setf (**preferred-addr** (*obj* *module*)) *new*) |



#### rebase_delta

| Language    | Get rebase_delta | Set rebase_delta |
|:------------|:-----------------|:-----------------|
| C++         | gtirb::Module::getRebaseDelta() | gtirb::Module::setRebaseDelta() |
| Python      | read gtirb.Module.rebase_delta  | write gtirb.Module.rebase_delta |
| Common Lisp | **rebase-delta** (*obj* *module*) => *result* | (setf (**rebase-delta** (*obj* *module*)) *new*) |

#### file_format

| Language    | Get file_format | Set file_format |
|:------------|:----------------|:----------------|
| C++         | gtirb::Module::getFileFormat() | gtirb::Module::setFileFormat() |
| Python      | read gtirb.Module.file_format  | write gtirb.Module.file_format  |
| Common Lisp | **file-format** (*obj* *module*) => *result* | (setf (**file-format** (*obj* *module*)) *new*) |

#### isa

| Language    | Get isa                 | Set isa                  |
|:------------|:------------------------|:-------------------------|
| C++         | gtirb::Module::getISA() | gtirb::Module::setISA()  |
| Python      | read gtirb.Module.isa   | write gtirb.Module.isa   |
| Common Lisp | **isa** (*obj* *module*) => *result* | (setf (**isa** (*obj* *module*)) *new*) |

#### name

| Language    | Get name                 | Set name                 |
|:------------|:-------------------------|:-------------------------|
| C++         | gtirb::Module::getName() | gtirb::Module::setName() |
| Python      | read gtirb.Module.name   | write gtirb.Module.name  |
| Common Lisp | **name** (*obj* *module*) => *result* | (setf (**name** (*obj* *module*)) *new*) |

#### sections

| Language    | Get sections               | Set sections                  |
|:------------|:---------------------------|:------------------------------|
| C++         | gtirb::Module::sections()  | gtirb::Module::addSection(), gtirb::Module::removeSection() |
| Python      | read gtirb.Module.sections | write gtirb.Module.sections |
| Common Lisp | **sections** (*object* *module*) => *result* | (setf (**sections** (*object* *module*)) *new-value*) |



#### symbols

| Language    | Get symbols               | Set symbols                |
|:------------|:--------------------------|:---------------------------|
| C++         | gtirb::Module::symbols()  | gtirb::Module::addSymbol(), gtirb::Module::removeSymbol |
| Python      | read gtirb.Module.symbols | write gtirb.Module.symbols |
| Common Lisp | **symbols** (*object* *module*) => *result* | (setf (**symbols** (*object* *module*)) *new-value*) |


#### proxy_blocks

| Language    | Get proxy_blocks              | Set proxy_blocks           |
|:------------|:------------------------------|:---------------------------|
| C++         | gtirb::Module::proxy_blocks() | gtirb::Module::addProxyBlock(), gtirb::Module::removeProxyBlock() |
| Python      | read gtirb.Module.proxies     | write gtirb.Module.proxies |
| Common Lisp | **proxies** (*object* *module*) => *result* | (setf (**proxies** (*object* *module*)) *new-value*)  |

#### entry_point

| Language    | Get entry_point | Set entry_point |
|:------------|:----------------|:----------------|
| C++         | gtirb::Module::getEntryPoint() | gtirb::Module::setEntryPoint() |
| Python      | read gtirb.Module.entry_point  | write gtirb.Module.entry_point  |
| Common Lisp | **entry-point** (*obj* *module*) => *result* | setf (**entry-point** (*obj* *module*)) *new-value*) |





### Associated IR


| Language    | Associated IR                             |
|:------------|:------------------------------------------|
| C++         | gtirb::Module::getIR()                    |
| Python      | gtirb.Module.ir                           |
| Common Lisp | **gtirb** (*object* *module*) => *result* |



### Find Section Objects...

| Language    | ...that intersect an address/range | ...that begin at an address/range
|:------------|:---------------------------|:---------------------------|
| C++         | gtirb::Module::findSectionsOn() | gtirb::Module::findSectionsAt() |
| Python      | gtirb.Module.sections_in() | gtirb.Module.sections_at() |
| Common Lisp | **in-address** *object* *start-address* &optional *end-address* => *result*, then filter *result* to extract the **section** objects | **at-address** *object* *address* => *result*, then filter *result* to extract the **section** objects. [*] |

[*] Address range checking is not yet implemented for Common Lisp
**at-address**
