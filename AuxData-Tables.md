Standard AuxData Table Schemata
==============================

The \ref AUXDATA_GROUP class provides generic storage for
application-specific data.

We specify a small number of standard gtirb::AuxData schemata to
support interoperability. These are listed below, in two sets:

- [Sanctioned](#sanctioned-auxdata-tables) Recommended for GTIRB users. Individual schemata are unlikely to change in future, although the set of Sanctioned schemata may grow.
- [Provisional](#provisional-auxdata-tables) Under consideration for 'sanctioned' status.


For example, if you want to store alignment requirements for blocks
and data objects, you can use an [alignment](#alignment) table.

```c++
using namespace gtirb;
Context C;
IR& ir = *IR::Create(C);

// Attach an empty alignment table to the internal representation
ir.addAuxData("alignment", std::map<UUID, uint64_t>{});
ir.addModule(Module::Create(C));
Module& module = ir.modules()[0];

//...

// Create a new block
CFG& cfg = module.getCFG();
Block* b1 = emplaceBlock(cfg, C, Addr(464), 6);

// Record that the block should be aligned to 8-byte boundaries.
ir.getAuxData("alignment")->get<std::map<UUID, uint64_t>>()[b1->getUUID()] = 8;
```



## Sanctioned AuxData Tables

The following are the sanctioned AuxData table schemata. 


| Label                            | Type                                                          | 
|----------------------------------|---------------------------------------------------------------|
| [`"function-information"`](#function-information)         | ```std::map<std::string,std::pair<std::set<gtirb::UUID>, std::set<gtirb::UUID>>>``` |
| [`"section-information"`](#section-information)           | FIXME                                       |
| [`"type-information-on-data"`](#type-information-on-data) | ```std::map<gtirb::UUID,std::string>```     |
| [`"alignment"`](@alignment)                               | ```std::map<gtirb::UUID, uint64_t>```       |
| [`"comments"`](#comments)                                 | ```std::map<std::pair<gtirb::UUID, uint64_t>, std::string>``` |
| [`"symbol-forwarding"`](#symbol-forwarding)               | ```std::map<gtirb::Symbol,gtirb::Symbol>``` |
| [`"padding"`](#padding)                                   | ```std::map<gtirb::Addr, uint64_t>```       |



### function-information

| <!-- -->    | <!-- -->    |
|-------------|-------------|
| Label | "function-information"                                                                         |
| Type  | ```std::map<std::string,std::pair<std::set<gtirb::UUID>, std::set<gtirb::UUID>>>```            |
| Key   | A function name.                                                                               |
| Value | A pair `(block_uuids, entry_uuids)` where `block_uuids` is the set of UUIDs of Blocks (gtirb::Block) included in the function  and `entry_uuids` is the set of UUIDs of function entry Blocks for the function. |


### section-information

| <!-- -->    | <!-- -->    |
|-------------|-------------|
| Label | "section-information" |
| Type  | FIXME                 |
| Key   | FIXME                 |
| Value | FIXME                 |


### type-information-on-data

| <!-- -->    | <!-- -->    |
|-------------|-------------|
| Label | ```"type-information-on-data"```                                                  |
| Type  | ```std::map<gtirb::UUID,std::string>```                                           |
| Key   | The gtirb::UUID of a gtirb::DataObject.                                           |
| Value | The type of the data, expressed as a std::string containing a C++ type specifier. |


### alignment

| <!-- -->    | <!-- -->    |
|-------------|-------------|
| Label | ```"alignment"```                                                        |
| Type  | ```std::map<gtirb::UUID, uint64_t>```                                    |
| Key   | The gtirb::UUID of a gtirb::Block, gtirb::DataObject, or gtirb::Section. |
| Value | Alignment requirements for the block/data object/section.                |


### comments

| <!-- -->    | <!-- -->    |
|-------------|-------------|
| Label | ```"comments"```                                                                |
| Type  |  ```gtirb::std::map<std::pair<gtirb::UUID, uint64_t>, std::string>>```          |
| Key   | A pair `(uuid, offset)` where `uuid` is the gtirb::UUID of a GTIRB entry and `offset` is an offset within that entry (in bytes). In the most typical use case, `uuid` will be the gtirb::UUID of a gtirb::Block and `offset` will be the offset of some instruction in that block. |
| Value | A comment string relevant to the specified offset in the specified GTIRB entry. |


### symbol-forwarding

| <!-- -->    | <!-- -->    |
|-------------|-------------|
| Label | ```"symbol-forwarding"```                    |
| Type  |  ```std::map<gtirb::Symbol,gtirb::Symbol>``` |
| Key   | The "from" gtirb::Symbol.                    |
| Value | The "to" gtirb::Symbol.                      |


### padding

| <!-- -->    | <!-- -->    |
|-------------|-------------|
| Label  | ```"padding"```                                |
| Type   | ``std::map<gtirb::Addr, uint64_t>```           |
| Key    | An address at which padding has been inserted. |
| Value  | The length of the padding, in bytes.           |



## Provisional AuxData Tables

There are currently no provisional table schemata.
