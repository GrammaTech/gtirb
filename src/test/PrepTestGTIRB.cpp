// This is a small utility program to generate a GTIRB file that can
// then be loaded by TestGTIRB. The fact this is a separate program
// allows us to test some aspects of the AuxDataContainer
// implementation that depend on different processes having different
// type schemas registered for AuxData types.

#include "AuxDataContainerSchema.hpp"
#include <gtirb/AuxDataContainer.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/IR.hpp>
#include <fstream>
#include <iostream>

void registerAuxDataTypes() {
  // This schema is *registered* here but unregistered in the unit test.
  gtirb::AuxDataContainer::registerAuxDataType<
      gtirb::schema::UnRegisteredType>();
}

int main(int argc, char* argv[]) {
  registerAuxDataTypes();

  // Name of the gtirb file to create is expected in argv[1].
  std::string GtirbFilename;
  if (argc >= 2) {
    GtirbFilename = argv[1];
  }

  if (GtirbFilename.empty()) {
    std::cerr << "*\n* No pre-built GTIRB file specified, cannot create GTIRB "
                 "test file!\n*\n";
    return -1;
  }

  std::ofstream GtirbFile;
  GtirbFile.open(GtirbFilename, std::ifstream::out);

  if (!GtirbFile) {
    std::cerr << "*\n* Failed to create GTIRB file: " << GtirbFilename
              << "\n*\n";
    return -1;
  }

  gtirb::Context Ctx;
  gtirb::IR* Ir = gtirb::IR::Create(Ctx);

  // Add content to the IR that the unit test can make use of.
  Ir->addAuxData<gtirb::schema::UnRegisteredType>(42);

  Ir->save(GtirbFile);

  return 0;
}
