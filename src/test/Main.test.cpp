#include "Main.test.hpp"
#include <gtirb/Context.hpp>
#include <gtirb/IR.hpp>
#include <chrono>
#include <fstream>
#include <gtest/gtest.h>
#include <string>
#include <thread>

void registerAuxDataContainerTestAuxDataTypes();
void registerIrTestAuxDataTypes();
void registerModuleTestAuxDataTypes();

static gtirb::Context Ctx;

// This is a GTIRB IR provided by the separate test program
// PrepTestGTIRB.
static gtirb::IR* TestIr = nullptr;

const gtirb::IR* getTestIr() { return TestIr; }

static void loadTestIr(std::string Filename) {
  std::ifstream GtirbFile;
  GtirbFile.open(Filename, std::ifstream::in | std::ifstream::binary);

  if (GtirbFile) {
    auto MaybeTestIr = gtirb::IR::load(Ctx, GtirbFile);
    if (MaybeTestIr) {
      TestIr = *MaybeTestIr;
    }
  }
  GtirbFile.close();
}

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
    loadTestIr(GtirbFilename);
  }

  ::testing::InitGoogleTest(&argc, argv);
  auto rv = RUN_ALL_TESTS();
  std::cerr << error_msgs.str();
  return rv;
}
