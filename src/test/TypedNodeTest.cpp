#include <gtest/gtest.h>
#include <proto/AddrRanges.pb.h>
#include <proto/Block.pb.h>
#include <proto/ByteMap.pb.h>
#include <proto/Data.pb.h>
#include <proto/IR.pb.h>
#include <proto/ImageByteMap.pb.h>
#include <proto/Instruction.pb.h>
#include <proto/Module.pb.h>
#include <proto/Relocation.pb.h>
#include <proto/Section.pb.h>
#include <proto/Symbol.pb.h>
#include <boost/uuid/uuid_generators.hpp>
#include <gtirb/AddrRanges.hpp>
#include <gtirb/Block.hpp>
#include <gtirb/Data.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Instruction.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/NodeReference.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <memory>

using testing::Types;

typedef Types<gtirb::AddrRanges,   //
              gtirb::Block,        //
              gtirb::Data,         //
              gtirb::IR,           //
              gtirb::ImageByteMap, //
              gtirb::Instruction,  //
              gtirb::Module,       //
              gtirb::Section,      //
              gtirb::Symbol        //
              >
    TypeImplementations;

// ----------------------------------------------------------------------------
// Typed test fixture.

template <class T> class TypedNodeTest : public testing::Test {
protected:
  TypedNodeTest() = default;
  virtual ~TypedNodeTest() = default;
};

TYPED_TEST_CASE_P(TypedNodeTest);

// ----------------------------------------------------------------------------
// Tests to run on all types.

TYPED_TEST_P(TypedNodeTest, ctor_0) { EXPECT_NO_THROW(TypeParam{}); }

TYPED_TEST_P(TypedNodeTest, uniqueUuids) {
  std::vector<gtirb::UUID> uuids;
  // Create a bunch of UUID's, then make sure we don't have any duplicates.

  for (size_t i = 0; i < 64; ++i) {
    const TypeParam n{};
    uuids.push_back(n.getUUID());
  }

  std::sort(std::begin(uuids), std::end(uuids));
  const auto end = std::unique(std::begin(uuids), std::end(uuids));

  EXPECT_EQ(std::end(uuids), end) << "Duplicate UUID's were generated.";
}

TYPED_TEST_P(TypedNodeTest, getByUUID) {
  TypeParam node;
  EXPECT_EQ(gtirb::Node::getByUUID(node.getUUID()), &node);
}

TYPED_TEST_P(TypedNodeTest, setUUIDUpdatesUUIDMap) {
  TypeParam node;
  auto oldId = node.getUUID();
  auto newId = boost::uuids::random_generator()();
  node.setUUID(newId);

  EXPECT_EQ(gtirb::Node::getByUUID(newId), &node);
  EXPECT_EQ(gtirb::Node::getByUUID(oldId), nullptr);
}

TYPED_TEST_P(TypedNodeTest, moveUpdatesUUIDMap) {
  TypeParam node1;
  auto id = node1.getUUID();
  TypeParam node2(std::move(node1));

  EXPECT_EQ(node2.getUUID(), id);
  EXPECT_EQ(gtirb::Node::getByUUID(id), &node2);
}

TYPED_TEST_P(TypedNodeTest, moveAssignmentUpdatesUUIDMap) {
  TypeParam node1;
  auto id = node1.getUUID();
  TypeParam node2 = std::move(node1);

  EXPECT_EQ(node2.getUUID(), id);
  EXPECT_EQ(gtirb::Node::getByUUID(id), &node2);
}

TYPED_TEST_P(TypedNodeTest, protobufUUIDRoundTrip) {
  typename TypeParam::MessageType message;
  gtirb::UUID origId;
  {
    TypeParam node1;
    origId = node1.getUUID();
    node1.toProtobuf(&message);
  }

  TypeParam node2;
  node2.fromProtobuf(message);

  EXPECT_EQ(node2.getUUID(), origId);
}

TYPED_TEST_P(TypedNodeTest, deserializeUpdatesUUIDMap) {
  gtirb::UUID id;
  typename TypeParam::MessageType message;

  {
    TypeParam node1;
    id = node1.getUUID();

    node1.toProtobuf(&message);
  }

  EXPECT_EQ(TypeParam::getByUUID(id), nullptr);

  TypeParam node2;
  node2.fromProtobuf(message);

  EXPECT_EQ(TypeParam::getByUUID(id), &node2);
}

TYPED_TEST_P(TypedNodeTest, nodeReference) {
  TypeParam node;
  gtirb::NodeReference<TypeParam> ref(node);

  TypeParam* ptr = ref;
  EXPECT_EQ(ptr, &node);
  EXPECT_EQ(ref->getUUID(), node.getUUID());
}

TYPED_TEST_P(TypedNodeTest, badReference) {
  TypeParam sym;
  gtirb::NodeReference<TypeParam> ref(gtirb::UUID{});

  TypeParam* ptr = ref;
  EXPECT_EQ(ptr, nullptr);
}

REGISTER_TYPED_TEST_CASE_P(TypedNodeTest,                //
                           protobufUUIDRoundTrip,        //
                           ctor_0,                       //
                           uniqueUuids,                  //
                           deserializeUpdatesUUIDMap,    //
                           getByUUID,                    //
                           setUUIDUpdatesUUIDMap,        //
                           moveUpdatesUUIDMap,           //
                           moveAssignmentUpdatesUUIDMap, //
                           nodeReference,                //
                           badReference);

INSTANTIATE_TYPED_TEST_CASE_P(Unit_Nodes,           // Instance name
                              TypedNodeTest,        // Test case name
                              TypeImplementations); // Type list
