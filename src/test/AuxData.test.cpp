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
#include "SerializationTestHarness.hpp"
#include <gtirb/AuxData.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/proto/AuxData.pb.h>
#include <gtest/gtest.h>
#include <memory>
#include <sstream>

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

struct PairOfCharAndInt64 {
  static constexpr const char* Name = "Pair of char and int64";
  typedef std::pair<char, int64_t> Type;
};

struct AnInt64 {
  static constexpr const char* Name = "A 64-bit integer";
  typedef int64_t Type;
};

struct AnInt32 {
  static constexpr const char* Name = "A 32-bit integer";
  typedef int32_t Type;
};

struct AnDouble {
  static constexpr const char* Name = "A 64-bit float";
  typedef double Type;
};

struct AnFloat {
  static constexpr const char* Name = "A 32-bit float";
  typedef float Type;
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

struct SimpleVariant {
  static constexpr const char* Name =
      "Simple variant of uint8_t, int32_t and Addr";
  typedef std::variant<uint8_t, int32_t, Addr> Type;
};

struct ComplexVariant {
  using Map = std::map<std::string, std::vector<int64_t>>;
  static constexpr const char* Name =
      "Complex variant of uint8_t, int32_t and complex map";
  typedef std::variant<uint8_t, int32_t, Map> Type;
};

struct DuplicateVariant {
  static constexpr const char* Name =
      "Variant with fields distinguished by position only";
  typedef std::variant<uint16_t, int16_t, uint16_t> Type;
};

struct MapCharToVariant {
  static constexpr const char* Name = "std::map<char,std::variant<Addr,char>>";
  typedef std::map<char, std::variant<Addr, char>> Type;
};

struct VecOfVariants {
  static constexpr const char* Name = "std::vector<std::variant<Addr, char>>";
  typedef std::vector<std::variant<Addr, char>> Type;
};

} // namespace schema
} // namespace gtirb

using namespace gtirb;
using namespace gtirb::schema;

static Context Ctx;

TEST(Unit_AuxData, eaMapProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  using MapT = std::map<Addr, std::string>;
  AuxDataImpl<MapAddrToString> Original =
      MapT({{Addr(1), {"a"}}, {Addr(2), {"b"}}});

  std::stringstream ss;
  STH::save(Original, ss);
  auto Intermediate = STH::load<AuxDataImpl<MapAddrToString>>(Ctx, ss);
  // Test that deserialized data can be reserialized again.
  std::stringstream ss2;
  STH::save(*Intermediate, ss2);
  auto Result = STH::load<AuxDataImpl<MapAddrToString>>(Ctx, ss2);

  const MapT* M = Result->get();
  EXPECT_TRUE(M);
  EXPECT_EQ(M->size(), 2);
  EXPECT_EQ(M->at(Addr(1)), "a");
  EXPECT_EQ(M->at(Addr(2)), "b");
}

TEST(Unit_AuxData, intMapProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  using MapT = std::map<int64_t, std::string>;
  AuxDataImpl<MapInt64ToString> Original = MapT({{1, {"a"}}, {2, {"b"}}});

  std::stringstream ss;
  STH::save(Original, ss);
  auto Result = STH::load<AuxDataImpl<MapInt64ToString>>(Ctx, ss);

  MapT M = *Result->get();
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(M[1], "a");
  EXPECT_EQ(M[2], "b");
}

TEST(Unit_AuxData, stringMapProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  using MapT = std::map<std::string, std::string>;
  AuxDataImpl<MapStringToString> Original = MapT({{"1", {"a"}}, {"2", {"b"}}});

  std::stringstream ss;
  STH::save(Original, ss);
  auto Result = STH::load<AuxDataImpl<MapStringToString>>(Ctx, ss);

  MapT M = *Result->get();
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(M["1"], "a");
  EXPECT_EQ(M["2"], "b");
}

TEST(Unit_AuxData, uuidMapProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  using MapT = std::map<UUID, std::string>;
  UUID Id1 = Node::Create(Ctx)->getUUID();
  UUID Id2 = Node::Create(Ctx)->getUUID();
  AuxDataImpl<MapUUIDToString> Original = MapT({{Id1, {"a"}}, {Id2, {"b"}}});

  std::stringstream ss;
  STH::save(Original, ss);
  auto Result = STH::load<AuxDataImpl<MapUUIDToString>>(Ctx, ss);

  MapT M = *Result->get();
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(M[Id1], "a");
  EXPECT_EQ(M[Id2], "b");
}

TEST(Unit_AuxData, mapVectorProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  auto Val = std::vector<std::map<std::string, int>>{{{"key", {1}}}};
  auto ValOrig = Val;

  AuxDataImpl<VectorMapStringToInt> Original(std::move(Val));
  std::stringstream ss;
  STH::save(Original, ss);
  auto Result = STH::load<AuxDataImpl<VectorMapStringToInt>>(Ctx, ss);

  auto New = *Result->get();
  EXPECT_EQ(New, ValOrig);
}

TEST(Unit_AuxData, eaVectorProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  AuxDataImpl<VectorAddr> Original =
      std::vector<Addr>({Addr(1), Addr(2), Addr(3)});

  std::stringstream ss;
  STH::save(Original, ss);
  auto Result = STH::load<AuxDataImpl<VectorAddr>>(Ctx, ss);

  EXPECT_EQ(*Result->get(), std::vector<Addr>({Addr(1), Addr(2), Addr(3)}));
}

TEST(Unit_AuxData, intVectorProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  AuxDataImpl<VectorInt64> Original = std::vector<int64_t>({1, 2, 3});

  std::stringstream ss;
  STH::save(Original, ss);
  auto Result = STH::load<AuxDataImpl<VectorInt64>>(Ctx, ss);

  EXPECT_EQ(*Result->get(), std::vector<int64_t>({1, 2, 3}));
}

TEST(Unit_AuxData, stringVectorProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  AuxDataImpl<VectorString> Original =
      std::vector<std::string>({"1", "2", "3"});

  std::stringstream ss;
  STH::save(Original, ss);
  auto Result = STH::load<AuxDataImpl<VectorString>>(Ctx, ss);

  EXPECT_EQ(*Result->get(), std::vector<std::string>({"1", "2", "3"}));
}

TEST(Unit_AuxData, uuidVectorProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  UUID Id1 = Node::Create(Ctx)->getUUID(), Id2 = Node::Create(Ctx)->getUUID(),
       Id3 = Node::Create(Ctx)->getUUID();
  AuxDataImpl<VectorUUID> Original = std::vector<UUID>({Id1, Id2, Id3});

  std::stringstream ss;
  STH::save(Original, ss);
  auto Result = STH::load<AuxDataImpl<VectorUUID>>(Ctx, ss);

  EXPECT_EQ(*Result->get(), std::vector<UUID>({Id1, Id2, Id3}));
}

TEST(Unit_AuxData, uuidSetProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  UUID Id1 = Node::Create(Ctx)->getUUID(), Id2 = Node::Create(Ctx)->getUUID(),
       Id3 = Node::Create(Ctx)->getUUID();
  AuxDataImpl<SetUUID> Original = std::set<UUID>({Id1, Id2, Id3});

  std::stringstream ss;
  STH::save(Original, ss);
  auto Result = STH::load<AuxDataImpl<SetUUID>>(Ctx, ss);

  EXPECT_EQ(*Result->get(), std::set<UUID>({Id1, Id2, Id3}));
  EXPECT_EQ(Result->rawData().ProtobufType, "set<UUID>");
}

TEST(Unit_AuxData, simpleVariantProtobufFirst) {
  using STH = gtirb::SerializationTestHarness;
  uint8_t ui = 1;
  std::variant<uint8_t, int32_t, Addr> Val(ui);
  auto ValOrig = Val;

  AuxDataImpl<SimpleVariant> Original(std::move(Val));
  std::stringstream ss;
  STH::save(Original, ss);

  auto Result = STH::load<AuxDataImpl<SimpleVariant>>(Ctx, ss);
  auto New = *Result->get();

  EXPECT_EQ(New, ValOrig);

  EXPECT_EQ(Result->rawData().ProtobufType, "variant<uint8_t,int32_t,Addr>");
}

TEST(Unit_AuxData, simpleVariantProtobufSecond) {
  using STH = gtirb::SerializationTestHarness;
  int32_t i = -1000;
  std::variant<uint8_t, int32_t, Addr> Val(i);
  auto ValOrig = Val;

  AuxDataImpl<SimpleVariant> Original(std::move(Val));
  std::stringstream ss;
  STH::save(Original, ss);

  auto Result = STH::load<AuxDataImpl<SimpleVariant>>(Ctx, ss);
  auto New = *Result->get();

  EXPECT_EQ(New, ValOrig);

  EXPECT_EQ(Result->rawData().ProtobufType, "variant<uint8_t,int32_t,Addr>");
}

TEST(Unit_AuxData, simpleVariantProtobufThird) {
  using STH = gtirb::SerializationTestHarness;
  Addr addr(0x1234);
  std::variant<uint8_t, int32_t, Addr> Val(addr);
  auto ValOrig = Val;

  AuxDataImpl<SimpleVariant> Original(std::move(Val));
  std::stringstream ss;
  STH::save(Original, ss);

  auto Result = STH::load<AuxDataImpl<SimpleVariant>>(Ctx, ss);
  auto New = *Result->get();

  EXPECT_EQ(New, ValOrig);

  EXPECT_EQ(Result->rawData().ProtobufType, "variant<uint8_t,int32_t,Addr>");
}

TEST(Unit_AuxData, complexVariantProtobufThird) {
  using STH = gtirb::SerializationTestHarness;
  using Map = std::map<std::string, std::vector<int64_t>>;
  Map M{{"a", {1, 2, 3}}};
  std::variant<uint8_t, int32_t, Map> Val(M);
  auto ValOrig = Val;

  AuxDataImpl<ComplexVariant> Original(std::move(Val));
  std::stringstream ss;
  STH::save(Original, ss);

  auto Result = STH::load<AuxDataImpl<ComplexVariant>>(Ctx, ss);
  auto New = *Result->get();

  EXPECT_EQ(New, ValOrig);

  EXPECT_EQ(Result->rawData().ProtobufType,
            "variant<uint8_t,int32_t,mapping<string,sequence<int64_t>>>");
}

TEST(Unit_AuxData, duplicateVariantProtobufFirst) {
  using STH = gtirb::SerializationTestHarness;
  std::variant<uint16_t, int16_t, uint16_t> Val{std::in_place_index<2>, 5};
  auto ValOrig = Val;
  AuxDataImpl<DuplicateVariant> Original(std::move(Val));
  std::stringstream ss;
  STH::save(Original, ss);

  auto Result = STH::load<AuxDataImpl<DuplicateVariant>>(Ctx, ss);
  auto New = *Result->get();
  EXPECT_EQ(New, ValOrig);
  EXPECT_EQ(New.index(), 2);
  EXPECT_EQ(Result->rawData().ProtobufType,
            "variant<uint16_t,int16_t,uint16_t>");
}

TEST(Unit_AuxData, VecOfVariants) {
  using STH = gtirb::SerializationTestHarness;
  using VariantT = std::variant<Addr, char>;
  AuxDataImpl<VecOfVariants> Original =
      std::vector<VariantT>{Addr(0xc0ffee), 'z', 'y', Addr(0xfeefaa)};
  std::stringstream ss;
  STH::save(Original, ss);
  auto Result = STH::load<AuxDataImpl<VecOfVariants>>(Ctx, ss);

  auto V = *Result->get();
  EXPECT_EQ(V.size(), 4);
  EXPECT_EQ(V[0], VariantT(0xc0ffee));
  EXPECT_EQ(V[2], VariantT('y'));
}

TEST(Unit_AuxData, MapCharToVariant) {
  using STH = gtirb::SerializationTestHarness;
  using VariantT = std::variant<Addr, char>;
  using MapT = std::map<char, VariantT>;

  AuxDataImpl<MapCharToVariant> Original =
      MapT({{'a', Addr(0x1111)}, {'b', 'x'}, {'z', Addr(0xdeadbeef)}});

  std::stringstream ss;
  STH::save(Original, ss);
  auto Result = STH::load<AuxDataImpl<MapCharToVariant>>(Ctx, ss);

  MapT M = *Result->get();
  EXPECT_EQ(M.size(), 3);
  EXPECT_EQ(M['a'], VariantT{Addr(0x1111)});
  EXPECT_EQ(M['b'], VariantT{'x'});
  EXPECT_EQ(M['z'], VariantT{Addr(0xdeadbeef)});
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

  X = auxdata_traits<std::variant<uint8_t, int32_t, Addr>>().type_name();
  EXPECT_EQ(X, "variant<uint8_t,int32_t,Addr>");

  using Map = std::map<std::string, std::vector<int64_t>>;
  X = auxdata_traits<std::variant<uint8_t, int32_t, Map>>().type_name();
  EXPECT_EQ(X, "variant<uint8_t,int32_t,mapping<string,sequence<int64_t>>>");
}

TEST(Unit_AuxData, getPrimitiveTypes) {
  AuxDataImpl<AChar> A = char('a');
  EXPECT_EQ(*A.get(), 'a');

  AuxDataImpl<AUint64> UI64 = uint64_t(123);
  EXPECT_EQ(*UI64.get(), 123);

  AuxDataImpl<AnInt64> SI64 = int64_t(-123);
  EXPECT_EQ(*SI64.get(), -123);

  AuxDataImpl<AnFloat> F32 = float(0.4000000059604645);
  EXPECT_EQ(*F32.get(), 0.4000000059604645);

  AuxDataImpl<AnDouble> F64 = float(1.0);
  EXPECT_EQ(*F64.get(), 1.0);

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
  using STH = gtirb::SerializationTestHarness;
  int64_t A = 123;
  auto Copy = A;
  AuxDataImpl<AnInt64> P(std::move(Copy));

  std::stringstream ss;
  STH::save(P, ss);
  auto Result = STH::load<AuxDataImpl<AnInt64>>(Ctx, ss);

  EXPECT_EQ(A, *Result->get());
}

TEST(Unit_AuxData, vectorProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  std::vector<int64_t> V({1, 2, 3});
  auto Copy = V;
  AuxDataImpl<VectorInt64> P(std::move(Copy));

  std::stringstream ss;
  STH::save(P, ss);
  auto Result = STH::load<AuxDataImpl<VectorInt64>>(Ctx, ss);

  EXPECT_EQ(*Result->get(), V);
}

TEST(Unit_AuxData, listProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  std::list<int64_t> V({1, 2, 3});
  auto Copy = V;
  AuxDataImpl<ListInt64> P(std::move(Copy));

  std::stringstream ss;
  STH::save(P, ss);
  auto Result = STH::load<AuxDataImpl<ListInt64>>(Ctx, ss);

  EXPECT_EQ(*Result->get(), V);
}

TEST(Unit_AuxData, stringProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  std::string S("abcd");
  auto Copy = S;
  AuxDataImpl<AString> P(std::move(Copy));

  std::stringstream ss;
  STH::save(P, ss);
  auto Result = STH::load<AuxDataImpl<AString>>(Ctx, ss);

  EXPECT_EQ(*Result->get(), S);
}

TEST(Unit_AuxData, addrProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  Addr A(0x1234);
  auto Copy = A;
  AuxDataImpl<AnAddr> P(std::move(Copy));

  std::stringstream ss;
  STH::save(P, ss);
  auto Result = STH::load<AuxDataImpl<AnAddr>>(Ctx, ss);

  EXPECT_EQ(*Result->get(), A);
}

TEST(Unit_AuxData, mapProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  std::map<char, int64_t> M({{'a', 1}, {'b', 2}, {'c', 3}});
  auto Copy = M;
  AuxDataImpl<MapCharToInt64> P(std::move(Copy));

  std::stringstream ss;
  STH::save(P, ss);
  auto Result = STH::load<AuxDataImpl<MapCharToInt64>>(Ctx, ss);

  EXPECT_EQ(*Result->get(), M);
}

TEST(Unit_AuxData, tupleProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  std::tuple<char, int64_t> T('a', 1);
  auto Copy = T;
  AuxDataImpl<TupleOfCharAndInt64> P(std::move(Copy));

  std::stringstream ss;
  STH::save(P, ss);
  auto Result = STH::load<AuxDataImpl<TupleOfCharAndInt64>>(Ctx, ss);

  EXPECT_EQ(*Result->get(), T);
}

TEST(Unit_AuxData, pairProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  std::pair<char, int64_t> Peer('a', 1);
  auto Copy = Peer;
  AuxDataImpl<PairOfCharAndInt64> P(std::move(Copy));

  std::stringstream ss;
  STH::save(P, ss);
  auto Result = STH::load<AuxDataImpl<PairOfCharAndInt64>>(Ctx, ss);

  EXPECT_EQ(*Result->get(), Peer);
}

TEST(Unit_AuxData, uuidProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  UUID Val = Node::Create(Ctx)->getUUID();
  auto Copy = Val;
  AuxDataImpl<AUUID> P(std::move(Copy));

  std::stringstream ss;
  STH::save(P, ss);
  auto Result = STH::load<AuxDataImpl<AUUID>>(Ctx, ss);

  EXPECT_EQ(*Result->get(), Val);
}

TEST(Unit_AuxData, OffsetProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  Offset Val{Node::Create(Ctx)->getUUID(), 123};
  auto Copy = Val;
  AuxDataImpl<AnOffset> P(std::move(Copy));

  std::stringstream ss;
  STH::save(P, ss);
  auto Result = STH::load<AuxDataImpl<AnOffset>>(Ctx, ss);

  auto NewVal = *Result->get();
  EXPECT_EQ(NewVal.ElementId, Val.ElementId);
  EXPECT_EQ(NewVal.Displacement, Val.Displacement);
}

TEST(Unit_AuxData, nestedProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;

  // Outer vector
  std::stringstream ss1;
  std::vector<std::map<char, std::tuple<int64_t, uint64_t>>> N1;
  N1.push_back({{'a', {0, 1}}, {'b', {2, 3}}});
  N1.push_back({{'c', {4, 5}}, {'d', {6, 7}}});
  auto Copy1 = N1;
  AuxDataImpl<VectorOfMapOfTuple> Original1 = std::move(Copy1);
  STH::save(Original1, ss1);
  auto Result1 = STH::load<AuxDataImpl<VectorOfMapOfTuple>>(Ctx, ss1);

  EXPECT_EQ(*Result1->get(), N1);

  // Outer map
  std::stringstream ss2;
  std::map<std::string, std::vector<int64_t>> N2{{"a", {1, 2, 3}}};
  auto Copy2 = N2;
  AuxDataImpl<MapOfVector> Original2 = std::move(Copy2);
  STH::save(Original2, ss2);
  auto Result2 = STH::load<AuxDataImpl<MapOfVector>>(Ctx, ss2);

  EXPECT_EQ(*Result2->get(), N2);

  // Outer tuple
  std::stringstream ss3;
  std::tuple<std::string, std::vector<int64_t>> N3{"a", {1, 2, 3}};
  auto Copy3 = N3;
  AuxDataImpl<TupleOfVector> Original3 = std::move(Copy3);
  STH::save(Original3, ss3);
  auto Result3 = STH::load<AuxDataImpl<TupleOfVector>>(Ctx, ss3);

  EXPECT_EQ(*Result3->get(), N3);
}

TEST(Unit_AuxData, wrongTypeAfterProtobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  AuxDataImpl<AnInt32> Original(1234);

  std::stringstream ss;
  STH::save(Original, ss);
  EXPECT_EQ(STH::load<AuxDataImpl<AString>>(Ctx, ss), nullptr);
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
