#include <gtest/gtest.h>

void registerAuxDataContainerTestAuxDataTypes();
void registerIrTestAuxDataTypes();
void registerModuleTestAuxDataTypes();

int main(int argc, char** argv) {

  // Register aux data types needed by testing
  registerAuxDataContainerTestAuxDataTypes();
  registerIrTestAuxDataTypes();
  registerModuleTestAuxDataTypes();

  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
