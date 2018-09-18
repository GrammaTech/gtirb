//===- TypedNodeTest.cpp ----------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2018 GrammaTech, Inc.
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
#include <gtirb/Block.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/DataObject.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/NodeRef.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <proto/Block.pb.h>
#include <proto/ByteMap.pb.h>
#include <proto/DataObject.pb.h>
#include <proto/IR.pb.h>
#include <proto/ImageByteMap.pb.h>
#include <proto/Module.pb.h>
#include <proto/Section.pb.h>
#include <proto/Symbol.pb.h>
#include <boost/uuid/uuid_generators.hpp>
#include <gtest/gtest.h>
#include <type_traits>

using testing::Types;

typedef Types<gtirb::DataObject*,   //
              gtirb::IR*,           //
              gtirb::ImageByteMap*, //
              gtirb::Module*,       //
              gtirb::Section*,      //
              gtirb::Symbol*        //
              >
    TypeImplementations;

static gtirb::Context Ctx;

// ----------------------------------------------------------------------------
// Typed test fixture.

template <class T> class TypedNodeTest : public testing::Test {
protected:
  TypedNodeTest() = default;
  virtual ~TypedNodeTest() = default;
};

TYPED_TEST_CASE_P(TypedNodeTest);

// I tried making this a member of TypedNodeTest, but the member is unavailable
// within the tests themselves, so this macro is used as a hacky solution.
#define Type std::remove_pointer_t<TypeParam>

// ----------------------------------------------------------------------------
// Tests to run on all types.

TYPED_TEST_P(TypedNodeTest, ctor_0) { EXPECT_NE(Type::Create(Ctx), nullptr); }

TYPED_TEST_P(TypedNodeTest, uniqueUuids) {
  std::vector<gtirb::UUID> Uuids;
  // Create a bunch of UUID's, then make sure we don't have any duplicates.

  for (size_t I = 0; I < 64; ++I) {
    const TypeParam N = Type::Create(Ctx);
    Uuids.push_back(N->getUUID());
  }

  std::sort(std::begin(Uuids), std::end(Uuids));
  const auto end = std::unique(std::begin(Uuids), std::end(Uuids));

  EXPECT_EQ(std::end(Uuids), end) << "Duplicate UUID's were generated.";
}

TYPED_TEST_P(TypedNodeTest, getByUUID) {
  TypeParam Node = Type::Create(Ctx);
  EXPECT_EQ(gtirb::Node::getByUUID(Ctx, Node->getUUID()), Node);
}

TYPED_TEST_P(TypedNodeTest, protobufUUIDRoundTrip) {
  typename Type::MessageType Message;
  gtirb::UUID OrigId;
  {
    gtirb::Context InnerCtx;
    TypeParam Node1 = Type::Create(InnerCtx);
    OrigId = Node1->getUUID();
    Node1->toProtobuf(&Message);
  }

  TypeParam Node2 = Type::fromProtobuf(Ctx, Message);
  EXPECT_EQ(Node2->getUUID(), OrigId);
}

TYPED_TEST_P(TypedNodeTest, deserializeUpdatesUUIDMap) {
  gtirb::UUID Id;
  typename Type::MessageType Message;

  {
    gtirb::Context InnerCtx;
    TypeParam Node1 = Type::Create(InnerCtx);
    Id = Node1->getUUID();

    Node1->toProtobuf(&Message);
  }

  EXPECT_EQ(Type::getByUUID(Ctx, Id), nullptr);

  TypeParam Node2 = Type::fromProtobuf(Ctx, Message);
  EXPECT_EQ(Type::getByUUID(Ctx, Id), Node2);
}

TYPED_TEST_P(TypedNodeTest, nodeReference) {
  TypeParam Node = Type::Create(Ctx);
  gtirb::NodeRef<Type> ref(Node);

  TypeParam Ptr = ref.get(Ctx);
  EXPECT_EQ(Ptr, Node);
  EXPECT_EQ(ref.get(Ctx)->getUUID(), Node->getUUID());
}

TYPED_TEST_P(TypedNodeTest, badReference) {
  gtirb::NodeRef<Type> Ref(gtirb::UUID{});

  TypeParam Ptr = Ref.get(Ctx);
  EXPECT_EQ(Ptr, nullptr);
}

REGISTER_TYPED_TEST_CASE_P(TypedNodeTest,             //
                           protobufUUIDRoundTrip,     //
                           ctor_0,                    //
                           uniqueUuids,               //
                           deserializeUpdatesUUIDMap, //
                           getByUUID,                 //
                           nodeReference,             //
                           badReference);

INSTANTIATE_TYPED_TEST_CASE_P(Unit_Nodes,           // Instance name
                              TypedNodeTest,        // Test case name
                              TypeImplementations); // Type list
