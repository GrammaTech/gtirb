#include "Main.test.hpp"
#include <gtirb/Context.hpp>
#include <gtirb/IR.hpp>
#include <fstream>
#include <gtest/gtest.h>
#include <string>

void registerAuxDataContainerTestAuxDataTypes();
void registerIrTestAuxDataTypes();
void registerModuleTestAuxDataTypes();

static gtirb::Context Ctx;

// This is a GTIRB IR provided by the separate test program
// PrepTestGTIRB.
static gtirb::IR* TestIr = nullptr;

const gtirb::IR* getTestIr() { return TestIr; }

int main(int argc, char** argv) {
  // Register aux data types needed by testing
  registerAuxDataContainerTestAuxDataTypes();
  registerIrTestAuxDataTypes();
  registerModuleTestAuxDataTypes();

  // Expect a gtirb filename passed as argv[1]
  std::string GtirbFilename;
  if (argc >= 2) {
    GtirbFilename = argv[1];
  }

  std::stringstream error_msgs;
  if (GtirbFilename.empty()) {
    error_msgs << "*\n* No pre-built GTIRB file specified, cross-process tests "
                  "will fail!\n*\n";
  } else {
    std::ifstream GtirbFile;
    GtirbFile.open(GtirbFilename, std::ifstream::in);

    if (GtirbFile) {
      auto MaybeTestIr = gtirb::IR::load(Ctx, GtirbFile);
      if (MaybeTestIr) {
        TestIr = *MaybeTestIr;
      }
    }

    if (!TestIr) {
      error_msgs << "*\n* Failed to load pre-built GTIRB file: "
                 << GtirbFilename << "\n";
      error_msgs << "* Cross-process tests will fail!\n*\n";
    }
  }

  ::testing::InitGoogleTest(&argc, argv);
  auto rv = RUN_ALL_TESTS();
  std::cerr << error_msgs.str();
  return rv;
}
