// An example program which opens an IR and prints information about every
// jump instruction.

#include <gtirb/gtirb.hpp>
#include <capstone/capstone.h>
#include <fstream>
#include <iomanip>
#include <iostream>

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
  IR* Ir = nullptr;

  if (argc == 2) {
    std::ifstream in(argv[1]);
    if (auto IoE = IR::load(C, in); IoE)
      Ir = *IoE;
  }

  if (!Ir)
    return EXIT_FAILURE;

  // Initialize capstone for decoding instructions.
  csh CsHandle;
  [[maybe_unused]] int Ret = cs_open(CS_ARCH_X86, CS_MODE_64, &CsHandle);
  assert(Ret == CS_ERR_OK);
  cs_option(CsHandle, CS_OPT_DETAIL, CS_OPT_ON);

  // Examine all blocks in the first module
  for (const auto& B : blocks(Ir->getCFG())) {
    // Get the contents of the block and decode with capstone
    cs_insn* Insn;
    size_t count =
        cs_disasm(CsHandle, B.rawBytes<uint8_t>(), B.getSize(),
                  (uint64_t)B.getAddress().value_or(Addr(0)), 0, &Insn);

    // Exception-safe cleanup of instructions
    std::unique_ptr<cs_insn, std::function<void(cs_insn*)>> freeInsn(
        Insn, [count](cs_insn* i) { cs_free(i, count); });

    // Examine all instructions in the block
    for (size_t I = 0; I < count; I++) {
      const auto& Inst = Insn[I];
      auto& Detail = *Inst.detail;
      for (int G = 0; G < Detail.groups_count; G++) {
        // Print jump instructions
        if (Detail.groups[G] == CS_GRP_JUMP) {
          std::cout << Addr(Inst.address) << ": " << Inst.mnemonic << "\t";
          auto& Op = Detail.x86.operands[0];
          if (Op.type == X86_OP_IMM) {
            std::cout << Addr(Op.imm) << "\n";
          } else {
            std::cout << "<indirect>\n";
          }
          break;
        }
      }
    }
  }
  return EXIT_SUCCESS;
}
