//===- AuxData.test.cpp ----------------------------------------*- C++-*-===//
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
#include <gtirb/AuxData.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/proto/AuxData.pb.h>
#include <gtest/gtest.h>
#include <memory>

struct MoveTest;

namespace gtirb {
namespace schema {

struct MapAddrToString {
  static constexpr const char* Name = "std::map<Addr, std::string>";
  typedef std::map<Addr, std::string> Type;
};

struct MapInt64ToString {
  static constexpr const char* Name = "std::map<int64_t, std::string>";
  typedef std::map<int64_t, std::string> Type;
};

struct MapStringToString {
  static constexpr const char* Name = "std::map<std::string, std::string>";
  typedef std::map<std::string, std::string> Type;
};

struct MapUUIDToString {
  static constexpr const char* Name = "std::map<UUID, std::string>";
  typedef std::map<UUID, std::string> Type;
};

struct VectorMapStringToInt {
  static constexpr const char* Name = "std::vector<std::map<std::string, int>>";
  typedef std::vector<std::map<std::string, int>> Type;
};

struct VectorAddr {
  static constexpr const char* Name = "std::vector<Addr>";
  typedef std::vector<Addr> Type;
};

struct VectorInt64 {
  static constexpr const char* Name = "std::vector<int64_t>";
  typedef std::vector<int64_t> Type;
};

struct VectorString {
  static constexpr const char* Name = "std::vector<std::string>";
  typedef std::vector<std::string> Type;
};

struct VectorUUID {
  static constexpr const char* Name = "std::vector<UUID>";
  typedef std::vector<UUID> Type;
};

struct SetUUID {
  static constexpr const char* Name = "std::set<UUID>";
  typedef std::set<UUID> Type;
};

struct AString {
  static constexpr const char* Name = "A string type";
  typedef std::string Type;
};

struct AnAddr {
  static constexpr const char* Name = "Addr";
  typedef Addr Type;
};

struct MapCharToInt64 {
  static constexpr const char* Name = "std::map<char, int64_t>";
  typedef std::map<char, int64_t> Type;
};

struct SetOfInt {
  static constexpr const char* Name = "Set of int";
  typedef std::set<int> Type;
};

struct TupleOfCharAndInt64 {
  static constexpr const char* Name = "Tuple of char and int64";
  typedef std::tuple<char, int64_t> Type;
};

struct AnInt64 {
  static constexpr const char* Name = "A 64-bit integer";
  typedef int64_t Type;
};

struct AnInt32 {
  static constexpr const char* Name = "A 32-bit integer";
  typedef int32_t Type;
};

struct ListInt64 {
  static constexpr const char* Name = "A list of 64-bit integers";
  typedef std::list<int64_t> Type;
};

struct AUUID {
  static constexpr const char* Name = "A UUID";
  typedef UUID Type;
};

struct AChar {
  static constexpr const char* Name = "A char";
  typedef char Type;
};

struct AUint64 {
  static constexpr const char* Name = "A 64-bit unsigned integer";
  typedef uint64_t Type;
};

struct AUint8 {
  static constexpr const char* Name = "An 8-bit unsigned integer";
  typedef uint8_t Type;
};

struct AnInt {
  static constexpr const char* Name = "A plain int";
  typedef int Type;
};

struct AnUnsigned {
  static constexpr const char* Name = "A plain unsigned";
  typedef unsigned Type;
};

struct AByte {
  static constexpr const char* Name = "A byte";
  typedef std::byte Type;
};

struct AnOffset {
  static constexpr const char* Name = "An offset";
  typedef Offset Type;
};

struct VectorOfMapOfTuple {
  static constexpr const char* Name = "A complex data structure";
  typedef std::vector<std::map<char, std::tuple<int64_t, uint64_t>>> Type;
};

struct MapOfVector {
  static constexpr const char* Name = "Another complex data structure";
  typedef std::map<std::string, std::vector<int64_t>> Type;
};

struct TupleOfVector {
  static constexpr const char* Name = "And another complex data structure";
  typedef std::tuple<std::string, std::vector<int64_t>> Type;
};

struct AMoveTest {
  static constexpr const char* Name = "A MoveTest";
  typedef MoveTest Type;
};

} // namespace schema
} // namespace gtirb

using namespace gtirb;
using namespace gtirb::schema;

static Context Ctx;

TEST(Unit_AuxData, eaMapProtobufRoundTrip) {
  using MapT = std::map<Addr, std::string>;
  AuxDataImpl<MapAddrToString> Original =
      MapT({{Addr(1), {"a"}}, {Addr(2), {"b"}}});

  AuxData::MessageType Message1;
  Original.toProtobuf(&Message1);
  auto Intermediate = AuxDataImpl<MapAddrToString>::fromProtobuf(Message1);
  // Test that deserialized data can be reserialized again.
  AuxData::MessageType Message2;
  Intermediate->toProtobuf(&Message2);
  auto Result = AuxDataImpl<MapAddrToString>::fromProtobuf(Message2);

  const MapT* M = Result->get();
  EXPECT_TRUE(M);
  EXPECT_EQ(M->size(), 2);
  EXPECT_EQ(M->at(Addr(1)), "a");
  EXPECT_EQ(M->at(Addr(2)), "b");
}

TEST(Unit_AuxData, intMapProtobufRoundTrip) {
  using MapT = std::map<int64_t, std::string>;
  AuxDataImpl<MapInt64ToString> Original = MapT({{1, {"a"}}, {2, {"b"}}});

  AuxData::MessageType Message;
  Original.toProtobuf(&Message);
  auto Result = AuxDataImpl<MapInt64ToString>::fromProtobuf(Message);

  MapT M = *Result->get();
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(M[1], "a");
  EXPECT_EQ(M[2], "b");
}

TEST(Unit_AuxData, stringMapProtobufRoundTrip) {
  using MapT = std::map<std::string, std::string>;
  AuxDataImpl<MapStringToString> Original = MapT({{"1", {"a"}}, {"2", {"b"}}});

  AuxData::MessageType Message;
  Original.toProtobuf(&Message);
  auto Result = AuxDataImpl<MapStringToString>::fromProtobuf(Message);

  MapT M = *Result->get();
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(M["1"], "a");
  EXPECT_EQ(M["2"], "b");
}

TEST(Unit_AuxData, uuidMapProtobufRoundTrip) {
  using MapT = std::map<UUID, std::string>;
  UUID Id1 = Node::Create(Ctx)->getUUID();
  UUID Id2 = Node::Create(Ctx)->getUUID();
  AuxDataImpl<MapUUIDToString> Original = MapT({{Id1, {"a"}}, {Id2, {"b"}}});

  AuxData::MessageType Message;
  Original.toProtobuf(&Message);
  auto Result = AuxDataImpl<MapUUIDToString>::fromProtobuf(Message);

  MapT M = *Result->get();
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(M[Id1], "a");
  EXPECT_EQ(M[Id2], "b");
}

TEST(Unit_AuxData, mapVectorProtobufRoundTrip) {
  auto Val = std::vector<std::map<std::string, int>>{{{"key", {1}}}};
  auto ValOrig = Val;

  AuxDataImpl<VectorMapStringToInt> Original(std::move(Val));
  AuxData::MessageType Message;
  Original.toProtobuf(&Message);
  auto Result = AuxDataImpl<VectorMapStringToInt>::fromProtobuf(Message);

  auto New = *Result->get();
  EXPECT_EQ(New, ValOrig);
}

TEST(Unit_AuxData, eaVectorProtobufRoundTrip) {
  AuxDataImpl<VectorAddr> Original =
      std::vector<Addr>({Addr(1), Addr(2), Addr(3)});

  AuxData::MessageType Message;
  Original.toProtobuf(&Message);
  auto Result = AuxDataImpl<VectorAddr>::fromProtobuf(Message);

  EXPECT_EQ(*Result->get(), std::vector<Addr>({Addr(1), Addr(2), Addr(3)}));
}

TEST(Unit_AuxData, intVectorProtobufRoundTrip) {
  AuxDataImpl<VectorInt64> Original = std::vector<int64_t>({1, 2, 3});

  AuxData::MessageType Message;
  Original.toProtobuf(&Message);
  auto Result = AuxDataImpl<VectorInt64>::fromProtobuf(Message);

  EXPECT_EQ(*Result->get(), std::vector<int64_t>({1, 2, 3}));
}

TEST(Unit_AuxData, stringVectorProtobufRoundTrip) {
  AuxDataImpl<VectorString> Original =
      std::vector<std::string>({"1", "2", "3"});

  AuxData::MessageType Message;
  Original.toProtobuf(&Message);
  auto Result = AuxDataImpl<VectorString>::fromProtobuf(Message);

  EXPECT_EQ(*Result->get(), std::vector<std::string>({"1", "2", "3"}));
}

TEST(Unit_AuxData, uuidVectorProtobufRoundTrip) {
  UUID Id1 = Node::Create(Ctx)->getUUID(), Id2 = Node::Create(Ctx)->getUUID(),
       Id3 = Node::Create(Ctx)->getUUID();
  AuxDataImpl<VectorUUID> Original = std::vector<UUID>({Id1, Id2, Id3});

  AuxData::MessageType Message;
  Original.toProtobuf(&Message);
  auto Result = AuxDataImpl<VectorUUID>::fromProtobuf(Message);

  EXPECT_EQ(*Result->get(), std::vector<UUID>({Id1, Id2, Id3}));
}

TEST(Unit_AuxData, uuidSetProtobufRoundTrip) {
  UUID Id1 = Node::Create(Ctx)->getUUID(), Id2 = Node::Create(Ctx)->getUUID(),
       Id3 = Node::Create(Ctx)->getUUID();
  AuxDataImpl<SetUUID> Original = std::set<UUID>({Id1, Id2, Id3});

  AuxData::MessageType Message;
  Original.toProtobuf(&Message);
  auto Result = AuxDataImpl<SetUUID>::fromProtobuf(Message);

  EXPECT_EQ(*Result->get(), std::set<UUID>({Id1, Id2, Id3}));
  EXPECT_EQ(Result->rawData().ProtobufType, "set<UUID>");
}

TEST(Unit_AuxData, auxdata_traits_type_name) {
  EXPECT_EQ(auxdata_traits<uint64_t>().type_name(), "uint64_t");
  EXPECT_EQ(auxdata_traits<std::vector<uint64_t>>().type_name(),
            "sequence<uint64_t>");
  std::string X = auxdata_traits<std::map<int64_t, uint64_t>>().type_name();
  EXPECT_EQ(X, "mapping<int64_t,uint64_t>");

  X = auxdata_traits<std::map<int64_t, std::vector<uint64_t>>>().type_name();
  EXPECT_EQ(X, "mapping<int64_t,sequence<uint64_t>>");

  X = auxdata_traits<std::vector<std::map<int64_t, uint64_t>>>().type_name();
  EXPECT_EQ(X, "sequence<mapping<int64_t,uint64_t>>");

  X = auxdata_traits<std::vector<std::tuple<int64_t, uint64_t>>>().type_name();
  EXPECT_EQ(X, "sequence<tuple<int64_t,uint64_t>>");

  X = auxdata_traits<std::set<int32_t>>().type_name();
  EXPECT_EQ(X, "set<int32_t>");
}

TEST(Unit_AuxData, getPrimitiveTypes) {
  AuxDataImpl<AChar> A = char('a');
  EXPECT_EQ(*A.get(), 'a');

  AuxDataImpl<AUint64> UI64 = uint64_t(123);
  EXPECT_EQ(*UI64.get(), 123);

  AuxDataImpl<AnInt64> SI64 = int64_t(-123);
  EXPECT_EQ(*SI64.get(), -123);

  AuxDataImpl<AUint8> UI8 = uint8_t(123);
  EXPECT_EQ(*UI8.get(), 123);

  AuxDataImpl<AnInt> SI = int(-123);
  EXPECT_EQ(*SI.get(), -123);

  AuxDataImpl<AnUnsigned> UI = unsigned(123);
  EXPECT_EQ(*UI.get(), 123);

  AuxDataImpl<AByte> B = std::byte(123);
  EXPECT_EQ(*B.get(), std::byte(123));
}

TEST(Unit_AuxData, getVector) {
  std::vector<int64_t> Orig({1, 2, 3});
  auto Copy = Orig;
  AuxDataImpl<VectorInt64> P(std::move(Copy));

  auto& result = *P.get();
  EXPECT_EQ(result, Orig);
}

TEST(Unit_AuxData, getString) {
  std::string Orig("abcd");
  AuxDataImpl<AString> P(std::move(Orig));

  EXPECT_EQ(*P.get(), "abcd");
}

TEST(Unit_AuxData, getAddr) {
  Addr Orig(0x1234);
  AuxDataImpl<AnAddr> P(std::move(Orig));

  EXPECT_EQ(*P.get(), Addr(0x1234));
}

TEST(Unit_AuxData, getMap) {
  std::map<char, int64_t> Orig({{'a', 1}, {'b', 2}, {'c', 3}});
  auto Copy = Orig;
  AuxDataImpl<MapCharToInt64> P(std::move(Copy));

  auto& Result = *P.get();
  EXPECT_EQ(Result, Orig);
}

TEST(Unit_AuxData, getSet) {
  std::set<int> Orig({1, 2, 3});
  auto Copy = Orig;
  AuxDataImpl<SetOfInt> P(std::move(Copy));

  auto& Result = *P.get();
  EXPECT_EQ(Result, Orig);
}

TEST(Unit_AuxData, getTuple) {
  std::tuple<char, int64_t> Orig('a', 1);
  auto Copy = Orig;
  AuxDataImpl<TupleOfCharAndInt64> P(std::move(Copy));

  auto& Result = *P.get();
  EXPECT_EQ(Result, Orig);
}

TEST(Unit_AuxData, protobufRoundTrip) {
  int64_t A = 123;
  auto Copy = A;
  AuxDataImpl<AnInt64> P(std::move(Copy));

  AuxData::MessageType Message;
  P.toProtobuf(&Message);
  auto Result = AuxDataImpl<AnInt64>::fromProtobuf(Message);

  EXPECT_EQ(A, *Result->get());
}

TEST(Unit_AuxData, vectorProtobufRoundTrip) {
  std::vector<int64_t> V({1, 2, 3});
  auto Copy = V;
  AuxDataImpl<VectorInt64> P(std::move(Copy));

  AuxData::MessageType Message;
  P.toProtobuf(&Message);
  auto Result = AuxDataImpl<VectorInt64>::fromProtobuf(Message);

  EXPECT_EQ(*Result->get(), V);
}

TEST(Unit_AuxData, listProtobufRoundTrip) {
  std::list<int64_t> V({1, 2, 3});
  auto Copy = V;
  AuxDataImpl<ListInt64> P(std::move(Copy));

  AuxData::MessageType Message;
  P.toProtobuf(&Message);
  auto Result = AuxDataImpl<ListInt64>::fromProtobuf(Message);

  EXPECT_EQ(*Result->get(), V);
}

TEST(Unit_AuxData, stringProtobufRoundTrip) {
  std::string S("abcd");
  auto Copy = S;
  AuxDataImpl<AString> P(std::move(Copy));

  AuxData::MessageType Message;
  P.toProtobuf(&Message);
  auto Result = AuxDataImpl<AString>::fromProtobuf(Message);

  EXPECT_EQ(*Result->get(), S);
}

TEST(Unit_AuxData, addrProtobufRoundTrip) {
  Addr A(0x1234);
  auto Copy = A;
  AuxDataImpl<AnAddr> P(std::move(Copy));

  AuxData::MessageType Message;
  P.toProtobuf(&Message);
  auto Result = AuxDataImpl<AnAddr>::fromProtobuf(Message);

  EXPECT_EQ(*Result->get(), A);
}

TEST(Unit_AuxData, mapProtobufRoundTrip) {
  std::map<char, int64_t> M({{'a', 1}, {'b', 2}, {'c', 3}});
  auto Copy = M;
  AuxDataImpl<MapCharToInt64> P(std::move(Copy));

  AuxData::MessageType Message;
  P.toProtobuf(&Message);
  auto Result = AuxDataImpl<MapCharToInt64>::fromProtobuf(Message);

  EXPECT_EQ(*Result->get(), M);
}

TEST(Unit_AuxData, tupleProtobufRoundTrip) {
  std::tuple<char, int64_t> T('a', 1);
  auto Copy = T;
  AuxDataImpl<TupleOfCharAndInt64> P(std::move(Copy));

  AuxData::MessageType Message;
  P.toProtobuf(&Message);
  auto Result = AuxDataImpl<TupleOfCharAndInt64>::fromProtobuf(Message);

  EXPECT_EQ(*Result->get(), T);
}

TEST(Unit_AuxData, uuidProtobufRoundTrip) {
  UUID Val = Node::Create(Ctx)->getUUID();
  auto Copy = Val;
  AuxDataImpl<AUUID> P(std::move(Copy));

  AuxData::MessageType Message;
  P.toProtobuf(&Message);
  auto Result = AuxDataImpl<AUUID>::fromProtobuf(Message);

  EXPECT_EQ(*Result->get(), Val);
}

TEST(Unit_AuxData, OffsetProtobufRoundTrip) {
  Offset Val{Node::Create(Ctx)->getUUID(), 123};
  auto Copy = Val;
  AuxDataImpl<AnOffset> P(std::move(Copy));

  AuxData::MessageType Message;
  P.toProtobuf(&Message);
  auto Result = AuxDataImpl<AnOffset>::fromProtobuf(Message);

  auto NewVal = *Result->get();
  EXPECT_EQ(NewVal.ElementId, Val.ElementId);
  EXPECT_EQ(NewVal.Displacement, Val.Displacement);
}

TEST(Unit_AuxData, nestedProtobufRoundTrip) {
  // Outer vector
  proto::AuxData Message1;
  std::vector<std::map<char, std::tuple<int64_t, uint64_t>>> N1;
  N1.push_back({{'a', {0, 1}}, {'b', {2, 3}}});
  N1.push_back({{'c', {4, 5}}, {'d', {6, 7}}});
  auto Copy1 = N1;
  AuxDataImpl<VectorOfMapOfTuple> Original1 = std::move(Copy1);
  Original1.toProtobuf(&Message1);
  auto Result1 = AuxDataImpl<VectorOfMapOfTuple>::fromProtobuf(Message1);

  EXPECT_EQ(*Result1->get(), N1);

  // Outer map
  proto::AuxData Message2;
  std::map<std::string, std::vector<int64_t>> N2{{"a", {1, 2, 3}}};
  auto Copy2 = N2;
  AuxDataImpl<MapOfVector> Original2 = std::move(Copy2);
  Original2.toProtobuf(&Message2);
  auto Result2 = AuxDataImpl<MapOfVector>::fromProtobuf(Message2);

  EXPECT_EQ(*Result2->get(), N2);

  // Outer tuple
  proto::AuxData Message3;
  std::tuple<std::string, std::vector<int64_t>> N3{"a", {1, 2, 3}};
  auto Copy3 = N3;
  AuxDataImpl<TupleOfVector> Original3 = std::move(Copy3);
  Original3.toProtobuf(&Message3);
  auto Result3 = AuxDataImpl<TupleOfVector>::fromProtobuf(Message3);

  EXPECT_EQ(*Result3->get(), N3);
}

TEST(Unit_AuxData, wrongTypeAfterProtobufRoundTrip) {
  AuxDataImpl<AnInt32> Original(1234);

  AuxData::MessageType Message;
  Original.toProtobuf(&Message);
  EXPECT_EQ(AuxDataImpl<AString>::fromProtobuf(Message), nullptr);
}

struct MoveTest {
  MoveTest() = default;
  MoveTest(int X) : Val(X) {}
  MoveTest(const MoveTest& other) : Val(other.Val) { CopyCount++; };
  MoveTest(MoveTest&& other) : Val(std::move(other.Val)) { MoveCount++; }
  static int CopyCount;
  static int MoveCount;
  int Val{0};
};
int MoveTest::CopyCount;
int MoveTest::MoveCount;

template <>
struct gtirb::auxdata_traits<MoveTest> : default_serialization<MoveTest> {
  static std::string type_name() { return "MoveTest"; }
};

TEST(Unit_AuxData, movesAndCopies) {
  MoveTest::CopyCount = 0;
  MoveTest::MoveCount = 0;

  MoveTest M(123);
  EXPECT_EQ(MoveTest::CopyCount, 0);
  EXPECT_EQ(MoveTest::MoveCount, 0);

  AuxDataImpl<AMoveTest> AD(std::move(M));
  EXPECT_EQ(AD.get()->Val, 123);
  EXPECT_EQ(MoveTest::CopyCount, 0);
  EXPECT_EQ(MoveTest::MoveCount, 1);
}
