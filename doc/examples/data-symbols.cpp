// An example program which opens an IR and prints information about all
// symbols pointing to data.

#include <gtirb/gtirb.hpp>
#include <fstream>
#include <iomanip>

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

  for (const auto& M : I->modules()) {
    std::cout << "Module " << M.getName() << "\n";
    // Examine all symbols in the module
    for (const auto& Sym : M.symbols()) {
      if (auto* Ref = Sym.getReferent<DataObject>(); Ref != nullptr) {
        // If the symbol refers to data, print some information about it
        std::cout << Sym.getName() << ":\t" << Ref->getAddress() << "\t"
                  << Ref->getSize() << " bytes\n";
      }
    }
  }
}
