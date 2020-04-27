Standard AuxData Schemata
=========================

The \ref AUXDATA_GROUP class provides generic storage for
application-specific data. This allows data to be attached to either
the IR or Module classes in GTIRB.

We specify a small number of standard gtirb::AuxData schemata to
support interoperability. These are listed below, in two sets:

- [Sanctioned](#sanctioned-auxdata-tables) Recommended for GTIRB
  users. Individual schemata are unlikely to change in future,
  although the set of Sanctioned schemata may grow.

- [Provisional](#provisional-auxdata-tables) Under consideration for
  'sanctioned' status.

For example, if you want to store alignment requirements for blocks
and data objects, you can use an [alignment](#alignment) table.

```c++
// Leverage definitions for the sanctioned AuxData tables.
#include <gtirb/AuxDataSchema.hpp>

// Define your own custom AuxData tables.
// By convention, we put these in the namespace gtirb::schema.
//
// Note that if the custom type requires custom serialization, a
// specialization of the auxdata_traits template also has to be
// provided. We provide default specializations for many standard
// types.
namespace gtirb {
namespace schema {
struct MyAuxDataFoo {
  static constexpr const char* Name = "foo";
  typedef Foo Type;
};
}
}

using namespace gtirb;
using namespace schema;

// Register AuxData types before using GTIRB.
void call_me_from_main()
{
  AuxDataContainer::registerAuxDataType<Alignment>();
  AuxDataContainer::registerAuxDataType<MyAuxDataFoo>();
}

void do_stuff_with_gtirb()
{
  Context C;
  IR& ir = *IR::Create(C);
  ir.addModule(Module::Create(C));
  Module& module = *ir.modules_begin();

  // Attach an empty alignment table to the internal representation
  module.addAuxData<Alignment>(std::map<UUID, uint64_t>{});

  //...

  // Create a new block
  Section* section = module.addSection(C, ".text");
  ByteInterval* interval = section->addByteInterval(C, Addr(400), 1000);
  CodeBlock* b1 = interval->addBlock<CodeBlock>(C, 64, 6);

  // Record that the block should be aligned to 8-byte boundaries.
  // First fetch the map AuxData.
  auto* align_map = module.getAuxData<Alignment>();

  // Check for null if you don't know that the module definitely has
  // an existing Alignment AuxData attached.
  if (align_map)
    (*align_map)[b1->getUUID()] = 8;

  // Attach a custom "Foo" object.
  // Note that AuxData uses a move reference
  Foo my_foo = BuildAFoo();
  module.addAuxData<MyAuxDataFoo>(std::move(my_foo));

  // Subsequently access the Foo table through the AuxData interface.
  module.getAuxData<MyAuxDataFoo>()->some_member_function();
}
```


## Sanctioned AuxData Tables

The following are the sanctioned AuxData table schemata.


| Label                                     | Type                                               |
|-------------------------------------------|----------------------------------------------------|
| [`"functionBlocks"`](#functionblocks)     | ```std::map<gtirb::UUID, std::set<gtirb::UUID>>``` |
| [`"functionEntries"`](#functionentries)   | ```std::map<gtirb::UUID, std::set<gtirb::UUID>>``` |
| [`"functionNames"`](#functionnames)       | ```std::map<gtirb::UUID, gtirb::UUID>```           |
| [`"types"`](#types)                       | ```std::map<gtirb::UUID, std::string>```           |
| [`"alignment"`](#alignment)               | ```std::map<gtirb::UUID, uint64_t>```              |
| [`"comments"`](#comments)                 | ```std::map<gtirb::Offset, std::string>```         |
| [`"symbolForwarding"`](#symbolforwarding) | ```std::map<gtirb::UUID, gtirb::UUID>```           |
| [`"padding"`](#padding)                   | ```std::map<gtirb::Offset, uint64_t>```              |


### functionBlocks

| <!-- --> | <!-- -->                                           |
|----------|----------------------------------------------------|
| Label    | ```"functionBlocks"```                             |
| Type     | ```std::map<gtirb::UUID, std::set<gtirb::UUID>>``` |
| Key      | Function UUID.                                     |
| Value    | The set of UUIDs of all the blocks (gtirb::Block) in the function. |
| AttachedTo | Module |


### functionEntries

| <!-- --> | <!-- -->                                           |
|----------|----------------------------------------------------|
| Label    | ```"functionEntries"```                            |
| Type     | ```std::map<gtirb::UUID, std::set<gtirb::UUID>>``` |
| Key      | Function UUID.                                     |
| Value    | The set of UUIDs of all the block (gtirb::Block) entry points for the function. |
| AttachedTo | Module |


### functionNames

| <!-- --> | <!-- -->                                                            |
|----------|---------------------------------------------------------------------|
| Label    | ```"functionNames"```                                               |
| Type     | ```std::map<gtirb::UUID, gtirb::UUID>```                            |
| Key      | Function UUID.                                                      |
| Value    | The UUID of a Symbol whose `name` field contains the name of the function. |
| AttachedTo | Module |


### types

| <!-- --> | <!-- -->                                |
|----------|-----------------------------------------|
| Label    | ```"types"```                           |
| Type     | ```std::map<gtirb::UUID,std::string>``` |
| Key      | The gtirb::UUID of a gtirb::DataObject. |
| Value    | The type of the data, expressed as a std::string containing a C++ type specifier. |
| AttachedTo | Module |


### alignment

| <!-- --> | <!-- -->                                                  |
|----------|-----------------------------------------------------------|
| Label    | ```"alignment"```                                         |
| Type     | ```std::map<gtirb::UUID, uint64_t>```                     |
| Key      | The gtirb::UUID of a gtirb::Block, gtirb::DataObject, or gtirb::Section. |
| Value    | Alignment requirements for the block/data object/section. |
| AttachedTo | Module |

### comments

| <!-- --> | <!-- -->                                   |
|----------|--------------------------------------------|
| Label    | ```"comments"```                           |
| Type     | ```std::map<gtirb::Offset, std::string>``` |
| Key      | The gtirb::Offset of a comment.            |
| Value    | A comment string relevant to the specified offset in the specified GTIRB entry. |
| AttachedTo | Module |


### symbolForwarding

| <!-- --> | <!-- -->                                     |
|----------|----------------------------------------------|
| Label    | ```"symbolForwarding"```                     |
| Type     | ```std::map<gtirb::UUID,gtirb::UUID>```      |
| Key      | The gtirb::UUID of the "from" gtirb::Symbol. |
| Value    | The gtirb::UUID of the "to" gtirb::Symbol.   |
| AttachedTo | Module |


### padding

| <!-- --> | <!-- -->                                       |
|----------|------------------------------------------------|
| Label    | ```"padding"```                                |
| Type     | ```std::map<gtirb::Addr, uint64_t>```          |
| Key      | An address at which padding has been inserted. |
| Value    | The length of the padding, in bytes.           |
| AttachedTo | Module |


## Provisional AuxData Tables

There are currently no provisional table schemata.
