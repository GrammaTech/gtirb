//===- TypedNodeTest.cpp ----------------------------------------*- C++ -*-===//
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
#include "SerializationTestHarness.hpp"
#include <gtirb/CodeBlock.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <gtirb/proto/IR.pb.h>
#include <gtirb/proto/Module.pb.h>
#include <gtirb/proto/Section.pb.h>
#include <gtirb/proto/Symbol.pb.h>
#include <boost/uuid/uuid_generators.hpp>
#include <gtest/gtest.h>
#include <sstream>
#include <type_traits>

using testing::Types;

typedef Types<gtirb::ByteInterval*, //
              gtirb::CodeBlock*,    //
              gtirb::DataBlock*,    //
              gtirb::IR*,           //
              gtirb::Module*,       //
              gtirb::ProxyBlock*,   //
              gtirb::Section*,      //
              gtirb::Symbol*        //
              >
    TypeImplementations;

static gtirb::Context Ctx;

// ----------------------------------------------------------------------------
// Helper for constructing nodes. Most nodes can be created with no arguments
// and can use the main template. But the template can be specialized for node
// types that require constructor arguments (e.g., Module).

template <class T> auto Create(gtirb::Context& C) { return T::Create(C); }

template <> auto Create<gtirb::Module>(gtirb::Context& C) {
  return gtirb::Module::Create(C, "test");
}

// ----------------------------------------------------------------------------
// Typed test fixture.

template <class T> class TypedNodeTest : public testing::Test {
protected:
  TypedNodeTest() = default;
  virtual ~TypedNodeTest() = default;
};

TYPED_TEST_SUITE_P(TypedNodeTest);

// I tried making this a member of TypedNodeTest, but the member is unavailable
// within the tests themselves, so this macro is used as a hacky solution.
#define Type std::remove_pointer_t<TypeParam>

// ----------------------------------------------------------------------------
// Tests to run on all types.

TYPED_TEST_P(TypedNodeTest, ctor_0) { EXPECT_NE(Create<Type>(Ctx), nullptr); }

TYPED_TEST_P(TypedNodeTest, uniqueUuids) {
  std::vector<gtirb::UUID> Uuids;
  // Create a bunch of UUID's, then make sure we don't have any duplicates.

  for (size_t I = 0; I < 64; ++I) {
    const TypeParam N = Create<Type>(Ctx);
    Uuids.push_back(N->getUUID());
  }

  std::sort(std::begin(Uuids), std::end(Uuids));
  const auto end = std::unique(std::begin(Uuids), std::end(Uuids));

  EXPECT_EQ(std::end(Uuids), end) << "Duplicate UUID's were generated.";
}

TYPED_TEST_P(TypedNodeTest, getByUUID) {
  TypeParam Node = Create<Type>(Ctx);
  EXPECT_EQ(gtirb::Node::getByUUID(Ctx, Node->getUUID()), Node);
}

TYPED_TEST_P(TypedNodeTest, protobufUUIDRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  std::stringstream ss;
  gtirb::UUID OrigId;
  {
    gtirb::Context InnerCtx;
    TypeParam Node1 = Create<Type>(InnerCtx);
    OrigId = Node1->getUUID();

    STH::save(*Node1, ss);
  }

  TypeParam Node2 = STH::load<Type>(Ctx, ss);
  EXPECT_EQ(Node2->getUUID(), OrigId);
}

TYPED_TEST_P(TypedNodeTest, deserializeUpdatesUUIDMap) {
  using STH = gtirb::SerializationTestHarness;
  gtirb::UUID Id;
  std::stringstream ss;

  {
    gtirb::Context InnerCtx;
    TypeParam Node1 = Create<Type>(InnerCtx);
    Id = Node1->getUUID();

    STH::save(*Node1, ss);
  }

  EXPECT_EQ(Type::getByUUID(Ctx, Id), nullptr);

  TypeParam Node2 = STH::load<Type>(Ctx, ss);
  EXPECT_EQ(Type::getByUUID(Ctx, Id), Node2);
}

REGISTER_TYPED_TEST_SUITE_P(TypedNodeTest,             //
                            protobufUUIDRoundTrip,     //
                            ctor_0,                    //
                            uniqueUuids,               //
                            deserializeUpdatesUUIDMap, //
                            getByUUID);

INSTANTIATE_TYPED_TEST_SUITE_P(Unit_Nodes,           // Instance name
                               TypedNodeTest,        // Test case name
                               TypeImplementations); // Type list
