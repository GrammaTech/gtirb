// Note: this file tests schema registration for AuxData. It is
// purposely built as a separate test program so that we can keep the
// type map unlocked. This means, one can write tests that register
// schema, but one should *not* write tests that actually involve
// constructing GTIRB IR.

// Note also: Because schema registration is global, keeping this file
// to a single unit test explicitly guarantees ordering in the state
// of the registration.

#include "AuxDataContainerSchema.hpp"
#include "PrepDeathTest.hpp"
#include <gtirb/AuxDataContainer.hpp>
#include <gtest/gtest.h>

using namespace gtirb;
using namespace schema;

#ifndef NDEBUG
TEST(Unit_AuxDataContainerDeathTest, SchemaRegistration) {
  AuxDataContainer::registerAuxDataType<RegisteredType>();

  // Able to re-register the same schema with no error.
  AuxDataContainer::registerAuxDataType<RegisteredType>();

  // Assertion if registering a second schema w/ duplicate name but
  // incompatibable type.
  {
    PrepDeathTest PDT;
    EXPECT_DEATH(AuxDataContainer::registerAuxDataType<DuplicateNameType>(),
                 "Different types registered for the same AuxData name.");
  }
}
#endif
