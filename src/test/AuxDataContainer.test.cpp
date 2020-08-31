//===- AuxDataContainer.test.cpp --------------------------------*- C++ -*-===//
//
//  Copyright (C) 2020 GrammaTech, Inc.
//
//  This code is licensed under the MIT license. See the LICENSE file in the
//  project root for license terms.
//
//  This project is sponsored by the Office of Naval Research, One Liberty
//  Center, 875 N. Randolph Street, Arlington, VA 22203 under contract #
//  N68335-17-C-0700.  The content of the information does not necessarily
//  reflect the position or policy of the Government and no official
//  endorsement should be inferred.
//
//===----------------------------------------------------------------------===//
#include "AuxDataContainerSchema.hpp"
#include "Main.test.hpp"
#include "PrepDeathTest.hpp"
#include "SerializationTestHarness.hpp"
#include <gtirb/AuxDataContainer.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/IR.hpp>
#include <gtest/gtest.h>
#include <memory>
#include <sstream>

// Note: Some things are not tested here, since they really need
// multiple processes to test correctly. In particular, it's difficult
// to test how the various methods are supposed to behave when an
// AuxData type is not registered even though a container has
// instances of the type (e.g. from some other process that did have
// those types registered.)

using namespace gtirb;
using namespace gtirb::schema;

Context Ctx;

void registerAuxDataContainerTestAuxDataTypes() {
  // Unfortunately, there doesn't seem to be an easy way to test the
  // error checks in registerAuxDataTypeInternal(). We can't check
  // them at this point because googletest isn't initialized yet. And
  // calling registerAuxData() in one of the test functions below
  // isn't guaranteed to occur before the type map gets locked by
  // serialization happening in one of the other unit tests.
  AuxDataContainer::registerAuxDataType<RegisteredType>();
  AuxDataContainer::registerAuxDataType<BadDeSerializationType>();
}

#ifndef NDEBUG
TEST(Unit_AuxDataContainerDeathTest, registerSchemaTooLate) {
  [[maybe_unused]] PrepDeathTest PDT;
  // The AuxData type map should be locked at this point due to the
  // loading of a pre-built GTIRB IR in Main.test.cpp, so we should
  // expect an assertion failure if we try to register a new schema at
  // this point.
  EXPECT_DEATH(AuxDataContainer::registerAuxDataType<UnRegisteredType>(),
               "New AuxData types cannot be added at this point.");
}
#endif

TEST(Unit_AuxDataContainer, addAuxDataRegistered) {
  using STH = gtirb::SerializationTestHarness;
  auto* Ir = IR::Create(Ctx);
  Ir->addAuxData<RegisteredType>(5);

  // Access it immediately?
  {
    const auto* CV =
        static_cast<const gtirb::IR*>(Ir)->getAuxData<RegisteredType>();
    EXPECT_NE(CV, nullptr);
    EXPECT_EQ(*CV, 5);
    auto* V = Ir->getAuxData<RegisteredType>();
    EXPECT_NE(V, nullptr);
    EXPECT_EQ(*V, 5);
  }

  std::stringstream ss;
  STH::save(*Ir, ss);
  Context ResultCtx;
  auto* Result = *STH::load<IR>(ResultCtx, ss);

  // Access it after serialization?
  {
    const auto* CV = Result->getAuxData<RegisteredType>();
    ASSERT_NE(CV, nullptr);
    EXPECT_EQ(*CV, 5);
    auto* V = Result->getAuxData<RegisteredType>();
    ASSERT_NE(V, nullptr);
    EXPECT_EQ(*V, 5);
  }
}

// Test that GTIRB correctly triggers an assertion failure when the client fails
// to register an AuxData schema.
#ifndef NDEBUG
TEST(Unit_AuxDataContainerDeathTest, addAuxDataUnregistered) {
  auto* Ir = IR::Create(Ctx);
  {
    [[maybe_unused]] PrepDeathTest PDT;
    EXPECT_DEATH(
        Ir->addAuxData<UnRegisteredType>(5),
        "Attempting to add AuxData with unregistered or incorrect type.");
  }
}
#endif

// Test that GTIRB correctly triggers an assertion failure when the client tries
// to use a schema that has the same name as a registered schema, but is
// actually a different type.
#ifndef NDEBUG
TEST(Unit_AuxDataContainerDeathTest, addAuxDataDuplicateName) {
  auto* Ir = IR::Create(Ctx);
  {
    [[maybe_unused]] PrepDeathTest PDT;
    EXPECT_DEATH(
        Ir->addAuxData<DuplicateNameType>(5),
        "Attempting to add AuxData with unregistered or incorrect type.");
  }
}
#endif

// Test that GTIRB correctly returns null when attempting to fetch
// AuxData that fails to unserialize.
TEST(Unit_AuxDataContainer, addAuxDataBadUnserialize) {
  using STH = gtirb::SerializationTestHarness;
  auto* Ir = IR::Create(Ctx);
  Ir->addAuxData<BadDeSerializationType>({5, 10});

  std::stringstream ss;
  STH::save(*Ir, ss);
  Context ResultCtx;
  auto* Result = *STH::load<IR>(ResultCtx, ss);

  // Access it after serialization?
  {
    const auto* CV = Result->getAuxData<RegisteredType>();
    EXPECT_EQ(CV, nullptr);
  }

  // Should still be present as raw data
  EXPECT_EQ(Result->getAuxDataSize(), 1);
  auto It = Result->aux_data_begin();
  const auto& Raw = *It;
  EXPECT_STREQ(Raw.Key.c_str(), BadDeSerializationType::Name);
  EXPECT_EQ(Raw.ProtobufType,
            auxdata_traits<schema::BadDeSerializationType::Type>::type_name());
  std::string ExpectedBytes = {0x5, 0x0, 0x0, 0x0, 0xA, 0x0, 0x0, 0x0};
  ASSERT_EQ(Raw.RawBytes.size(), ExpectedBytes.size());
  EXPECT_EQ(Raw.RawBytes, ExpectedBytes);
}

// AuxData not present
TEST(Unit_AuxDataContainer, getAuxDataNotPresent) {
  auto* Ir = IR::Create(Ctx);
  EXPECT_EQ(Ir->getAuxData<RegisteredType>(), nullptr);
}

// AuxData present, but accessed w/ incompatible schema
#ifndef NDEBUG
TEST(Unit_AuxDataContainerDeathTest, getAuxDataIncompatibleSchema) {
  auto* Ir = IR::Create(Ctx);
  Ir->addAuxData<RegisteredType>(5);
  {
    [[maybe_unused]] PrepDeathTest PDT;
    EXPECT_DEATH(Ir->getAuxData<DuplicateNameType>(),
                 "Attempting to retrieve AuxData with incorrect type.");
  }
}
#endif

// AuxData present, but accessed w/ unregistered schema
#ifndef NDEBUG
TEST(Unit_AuxDataContainerDeathTest, getAuxDataUnregisteredType) {
  const auto* Ir = getTestIr();
  {
    [[maybe_unused]] PrepDeathTest PDT;
    EXPECT_DEATH(Ir->getAuxData<UnRegisteredType>(),
                 "Attempting to retrieve AuxData with an unregistered type.");
  }
}
#endif

// Removing AuxData by schema
TEST(Unit_AuxDataContainer, removeAuxDataBySchema) {
  auto* Ir = IR::Create(Ctx);
  Ir->addAuxData<RegisteredType>(5);
  const auto* CV = Ir->getAuxData<RegisteredType>();
  ASSERT_NE(CV, nullptr);
  EXPECT_EQ(*CV, 5);
  EXPECT_TRUE(Ir->removeAuxData<RegisteredType>());
  EXPECT_EQ(Ir->getAuxData<RegisteredType>(), nullptr);
  EXPECT_FALSE(Ir->removeAuxData<RegisteredType>());
}

// Removing AuxData by name
TEST(Unit_AuxDataContainer, removeAuxDataByName) {
  auto* Ir = IR::Create(Ctx);
  Ir->addAuxData<RegisteredType>(5);
  const auto* CV = Ir->getAuxData<RegisteredType>();
  ASSERT_NE(CV, nullptr);
  EXPECT_EQ(*CV, 5);
  EXPECT_TRUE(Ir->removeAuxData("registered type"));
  EXPECT_EQ(Ir->getAuxData<RegisteredType>(), nullptr);
  EXPECT_FALSE(Ir->removeAuxData("registered type"));
}

// Removing unregistered AuxData
#ifndef NDEBUG
TEST(Unit_AuxDataContainerDeathTest, removeAuxDataUnregistered) {
  auto* Ir = IR::Create(Ctx);
  {
    [[maybe_unused]] PrepDeathTest PDT;
    EXPECT_DEATH(Ir->removeAuxData<UnRegisteredType>(),
                 "Attempting to remove AuxData with an unregistered type.");
  }
}
#endif

// Iteration and container size
TEST(Unit_AuxDataContainer, iteration) {
  auto* Ir = IR::Create(Ctx);

  EXPECT_TRUE(Ir->getAuxDataEmpty());
  EXPECT_EQ(Ir->getAuxDataSize(), 0);

  Ir->addAuxData<RegisteredType>(5);
  Ir->addAuxData<BadDeSerializationType>({10, 20});

  EXPECT_FALSE(Ir->getAuxDataEmpty());
  EXPECT_EQ(Ir->getAuxDataSize(), 2);

  // begin/end
  bool SawRegistered = false;
  bool SawBad = false;
  for (auto It = Ir->aux_data_begin(); It != Ir->aux_data_end(); ++It) {
    if (It->Key == RegisteredType::Name) {
      SawRegistered = true;
    } else if (It->Key == BadDeSerializationType::Name) {
      SawBad = true;
    } else {
      EXPECT_TRUE(false);
    }
  }
  EXPECT_TRUE(SawRegistered);
  EXPECT_TRUE(SawBad);

  // Check that the range version gives us the same start and end
  // points.
  EXPECT_EQ(Ir->aux_data_begin(), Ir->aux_data().begin());
  EXPECT_EQ(Ir->aux_data_end(), Ir->aux_data().end());

  // Remove everything.
  Ir->clearAuxData();
  EXPECT_TRUE(Ir->getAuxDataEmpty());
  EXPECT_EQ(Ir->getAuxDataSize(), 0);
  EXPECT_EQ(Ir->aux_data_begin(), Ir->aux_data_end());
  EXPECT_TRUE(Ir->aux_data().empty());
}
