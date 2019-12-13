// An example program which opens and IR and prints information about
// functions.

#include <gtirb/gtirb.hpp>
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

int main(int argc, char** argv) {
  // Create a context to manage memory for gtirb objects
  Context C;

  // Load the IR
  IR* I;

  assert(argc == 2);
  {
    std::ifstream in(argv[1]);
    I = IR::load(C, in);
  }

  // Build a map of basic blocks by address
  std::map<Addr, const CodeBlock*> BlocksByAddr;
  auto Blocks = blocks(I->getCFG());
  std::for_each(Blocks.begin(), Blocks.end(), [&BlocksByAddr](const auto& B) {
    if (std::optional<Addr> A = B.getAddress())
      BlocksByAddr.emplace(*A, &B);
  });

  // Load function information from AuxData.
  // This information is not guaranteed to be present. For the purposes of
  // this example we assume that it exists, but real code should check for
  // nullptr in the return value of getAuxData and get.
  auto& Functions =
      *(I->getAuxData("functions")
            ->get<std::vector<std::tuple<std::string, Addr, uint64_t>>>());

  // Print function information
  for (auto& [Name, Address, Size] : Functions) {
    Addr EndAddr = Address + Size;
    std::cout << Name << "\t" << Address << "-" << EndAddr;

    // Examine all blocks in the function, looking for calls.
    auto End = BlocksByAddr.end();
    int CallCount = 0;
    for (auto It = BlocksByAddr.find(Address); It != End && It->first < EndAddr;
         It++) {
      // FIXME: implement in terms of edge labels once those are available
    }
    std::cout << ", contains " << CallCount << " calls\n";
  }
}
