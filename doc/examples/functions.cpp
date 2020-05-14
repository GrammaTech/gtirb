// An example program which opens and IR and prints information about
// functions.

#include <gtirb/gtirb.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>

using namespace gtirb;

// Print Addrs in hex format
std::ostream& operator<<(std::ostream& Os, Addr A) {
  auto Flags = Os.flags();
  Os << "0x" << std::hex << std::setw(8) << std::setfill('0') << uint64_t(A);
  Os.flags(Flags);
  return Os;
}

void register_aux_data_types() {
  using namespace gtirb::schema;
  AuxDataContainer::registerAuxDataType<FunctionEntries>();
  AuxDataContainer::registerAuxDataType<FunctionBlocks>();
}

int main(int argc, char** argv) {
  // Register the AuxData we'll want to use.
  register_aux_data_types();

  // Create a context to manage memory for gtirb objects
  Context C;

  // Load the IR
  IR* I = nullptr;

  if (argc == 2) {
    std::ifstream in(argv[1]);
    I = IR::load(C, in);
  }

  if (!I)
    return EXIT_FAILURE;

  // Load function information from AuxData.
  // This information is not guaranteed to be present. For the purposes of
  // this example we assume that it exists, but real code should check for
  // nullptr in the return value of getAuxData.
  auto& FunctionEntries =
      *(I->modules_begin()->getAuxData<gtirb::schema::FunctionEntries>());
  auto& FunctionBlocks =
      *(I->modules_begin()->getAuxData<gtirb::schema::FunctionBlocks>());

  // Print function information
  for (auto& [Function, Entries] : FunctionEntries) {

    // Note: this prints out the function's UUID.
    std::cout << boost::uuids::to_string(Function) << "\n";

    // Print information about entry points.
    // TODO: add symbols
    std::cout << "  Entries:\n";
    for (auto EntryUUID : Entries) {
      auto EntryNode = Node::getByUUID(C, EntryUUID);
      assert(EntryNode);
      auto EntryBlock = dyn_cast_or_null<CodeBlock>(EntryNode);
      assert(EntryBlock);
      std::cout << "    " << EntryBlock->getAddress() << "\n";
    }

    // Examine all blocks in the function.
    std::cout << "  Blocks:\n";
    auto It = FunctionBlocks.find(Function);
    assert(It != FunctionBlocks.end());
    auto& Blocks = It->second;
    for (auto BlockUUID : Blocks) {
      auto BlockNode = Node::getByUUID(C, BlockUUID);
      assert(BlockNode);
      auto Block = dyn_cast_or_null<CodeBlock>(BlockNode);
      assert(Block);
      std::cout << "    " << Block->getAddress() << "\n";
    }
  }
  return EXIT_SUCCESS;
}
