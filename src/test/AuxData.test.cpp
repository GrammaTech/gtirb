//===- AuxData.test.cpp ----------------------------------------*- C++-*-===//
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
#include <gtirb/AuxData.hpp>
#include <gtirb/Context.hpp>
#include <proto/AuxData.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_AuxData, eaMapProtobufRoundTrip) {
  using MapT = std::map<Addr, std::string>;
  AuxData Original = MapT({{Addr(1), {"a"}}, {Addr(2), {"b"}}});

  gtirb::AuxData Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = *Result.get<MapT>();
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(M[Addr(1)], "a");
  EXPECT_EQ(M[Addr(2)], "b");
}

TEST(Unit_AuxData, intMapProtobufRoundTrip) {
  using MapT = std::map<int64_t, std::string>;
  AuxData Original = MapT({{1, {"a"}}, {2, {"b"}}});

  gtirb::AuxData Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = *Result.get<MapT>();
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(M[1], "a");
  EXPECT_EQ(M[2], "b");
}

TEST(Unit_AuxData, stringMapProtobufRoundTrip) {
  using MapT = std::map<std::string, std::string>;
  AuxData Original = MapT({{"1", {"a"}}, {"2", {"b"}}});

  gtirb::AuxData Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = *Result.get<MapT>();
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(M["1"], "a");
  EXPECT_EQ(M["2"], "b");
}

TEST(Unit_AuxData, uuidMapProtobufRoundTrip) {
  using MapT = std::map<UUID, std::string>;
  UUID Id1 = Node::Create(Ctx)->getUUID();
  UUID Id2 = Node::Create(Ctx)->getUUID();
  AuxData Original = MapT({{Id1, {"a"}}, {Id2, {"b"}}});

  gtirb::AuxData Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = *Result.get<MapT>();
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(M[Id1], "a");
  EXPECT_EQ(M[Id2], "b");
}

TEST(Unit_AuxData, mapVectorProtobufRoundTrip) {
  auto Val = std::vector<std::map<std::string, int>>{{{"key", {1}}}};

  AuxData Original(Val);
  gtirb::AuxData Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  auto New = *Result.get<decltype(Val)>();
  EXPECT_EQ(New, Val);
}

TEST(Unit_AuxData, eaVectorProtobufRoundTrip) {
  AuxData Original = std::vector<Addr>({Addr(1), Addr(2), Addr(3)});

  gtirb::AuxData Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(*Result.get<std::vector<Addr>>(),
            std::vector<Addr>({Addr(1), Addr(2), Addr(3)}));
}

TEST(Unit_AuxData, intVectorProtobufRoundTrip) {
  AuxData Original = std::vector<int64_t>({1, 2, 3});

  gtirb::AuxData Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(*Result.get<std::vector<int64_t>>(),
            std::vector<int64_t>({1, 2, 3}));
}

TEST(Unit_AuxData, stringVectorProtobufRoundTrip) {
  AuxData Original = std::vector<std::string>({"1", "2", "3"});

  gtirb::AuxData Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(*Result.get<std::vector<std::string>>(),
            std::vector<std::string>({"1", "2", "3"}));
}

TEST(Unit_AuxData, uuidVectorProtobufRoundTrip) {
  UUID Id1 = Node::Create(Ctx)->getUUID(), Id2 = Node::Create(Ctx)->getUUID(),
       Id3 = Node::Create(Ctx)->getUUID();
  AuxData Original = std::vector<UUID>({Id1, Id2, Id3});

  gtirb::AuxData Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(*Result.get<std::vector<UUID>>(),
            std::vector<UUID>({Id1, Id2, Id3}));
}

TEST(Unit_AuxData, typeName) {
  EXPECT_EQ(AuxDataTemplate<uint64_t>().typeName(), "uint64_t");
  EXPECT_EQ(AuxDataTemplate<std::vector<uint64_t>>().typeName(),
            "sequence<uint64_t>");
  std::string X = AuxDataTemplate<std::map<int64_t, uint64_t>>().typeName();
  EXPECT_EQ(X, "mapping<int64_t,uint64_t>");

  X = AuxDataTemplate<std::map<int64_t, std::vector<uint64_t>>>().typeName();
  EXPECT_EQ(X, "mapping<int64_t,sequence<uint64_t>>");

  X = AuxDataTemplate<std::vector<std::map<int64_t, uint64_t>>>().typeName();
  EXPECT_EQ(X, "sequence<mapping<int64_t,uint64_t>>");

  X = AuxDataTemplate<std::vector<std::tuple<int64_t, uint64_t>>>().typeName();
  EXPECT_EQ(X, "sequence<tuple<int64_t,uint64_t>>");
}

TEST(Unit_AuxData, getPrimitiveTypes) {
  AuxData P;
  P = char('a');
  EXPECT_EQ(*P.get<char>(), 'a');

  P = uint64_t(123);
  EXPECT_EQ(*P.get<uint64_t>(), 123);

  P = int64_t(-123);
  EXPECT_EQ(*P.get<int64_t>(), -123);

  P = uint8_t(123);
  EXPECT_EQ(*P.get<uint8_t>(), 123);

  P = int(-123);
  EXPECT_EQ(*P.get<int>(), -123);

  P = unsigned(123);
  EXPECT_EQ(*P.get<unsigned>(), 123);

  P = std::byte(123);
  EXPECT_EQ(*P.get<std::byte>(), std::byte(123));
}

TEST(Unit_AuxData, getVector) {
  std::vector<int64_t> Orig({1, 2, 3});
  AuxData P(Orig);

  auto& result = *P.get<std::vector<int64_t>>();
  EXPECT_EQ(result, Orig);
}

TEST(Unit_AuxData, getString) {
  std::string Orig("abcd");
  AuxData P(Orig);

  EXPECT_EQ(*P.get<std::string>(), "abcd");
}

TEST(Unit_AuxData, getAddr) {
  Addr Orig(0x1234);
  AuxData P(Orig);

  EXPECT_EQ(*P.get<Addr>(), Addr(0x1234));
}

TEST(Unit_AuxData, getMap) {
  std::map<char, int64_t> Orig({{'a', 1}, {'b', 2}, {'c', 3}});
  AuxData P(Orig);

  auto& Result = *P.get<std::map<char, int64_t>>();
  EXPECT_EQ(Result, Orig);
}

TEST(Unit_AuxData, getTuple) {
  std::tuple<char, int64_t> Orig('a', 1);
  AuxData P(Orig);

  auto& Result = *P.get<std::tuple<char, int64_t>>();
  EXPECT_EQ(Result, Orig);
}

TEST(Unit_AuxData, getWrongType) {
  AuxData AuxData('a');
  EXPECT_EQ(AuxData.get<int>(), nullptr);
}

TEST(Unit_AuxData, getEmpty) {
  AuxData Empty;
  EXPECT_EQ(Empty.get<int>(), nullptr);
}

TEST(Unit_AuxData, getWrongContainer) {
  AuxData P;
  P = std::vector<int>();
  EXPECT_EQ(P.get<std::list<int>>(), nullptr);
}

TEST(Unit_AuxData, protobufRoundTrip) {
  int64_t A = 123;
  AuxData Original;
  Original = A;

  auto Message = toProtobuf(Original);
  AuxData Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(A, *Result.get<int64_t>());
}

TEST(Unit_AuxData, vectorProtobufRoundTrip) {
  std::vector<int64_t> V({1, 2, 3});
  AuxData Original;
  Original = V;

  auto Message = toProtobuf(Original);
  AuxData Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(*Result.get<decltype(V)>(), V);
}

TEST(Unit_AuxData, listProtobufRoundTrip) {
  std::list<int64_t> V({1, 2, 3});
  AuxData Original;
  Original = V;

  auto Message = toProtobuf(Original);
  AuxData Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(*Result.get<decltype(V)>(), V);
}

TEST(Unit_AuxData, listToVectorProtobufRoundTrip) {
  std::list<int64_t> Lst({1, 2, 3});
  AuxData Original;
  Original = Lst;

  auto Message = toProtobuf(Original);
  AuxData Result;
  fromProtobuf(Ctx, Result, Message);

  auto vec = std::vector<int64_t>(Lst.begin(), Lst.end());
  EXPECT_EQ(*Result.get<decltype(vec)>(), vec);
}

TEST(Unit_AuxData, stringProtobufRoundTrip) {
  std::string S("abcd");
  AuxData Original;
  Original = S;

  auto Message = toProtobuf(Original);
  AuxData Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(*Result.get<decltype(S)>(), S);
}

TEST(Unit_AuxData, addrProtobufRoundTrip) {
  Addr A(0x1234);
  AuxData Original;
  Original = A;

  auto Message = toProtobuf(Original);
  AuxData Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(*Result.get<decltype(A)>(), A);
}

TEST(Unit_AuxData, mapProtobufRoundTrip) {
  std::map<char, int64_t> M({{'a', 1}, {'b', 2}, {'c', 3}});
  AuxData Original;
  Original = M;

  auto Message = toProtobuf(Original);
  AuxData Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(*Result.get<decltype(M)>(), M);
}

TEST(Unit_AuxData, tupleProtobufRoundTrip) {
  std::tuple<char, int64_t> T('a', 1);
  AuxData Original;
  Original = T;

  auto Message = toProtobuf(Original);
  AuxData Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(*Result.get<decltype(T)>(), T);
}

TEST(Unit_AuxData, uuidProtobufRoundTrip) {
  UUID Val = Node::Create(Ctx)->getUUID();
  AuxData Original;
  Original = Val;

  auto Message = toProtobuf(Original);
  AuxData Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(*Result.get<decltype(Val)>(), Val);
}

TEST(Unit_AuxData, instructionRefProtobufRoundTrip) {
  InstructionRef Val{Node::Create(Ctx)->getUUID(), 123};
  AuxData Original;
  Original = Val;

  auto Message = toProtobuf(Original);
  AuxData Result;
  fromProtobuf(Ctx, Result, Message);

  auto NewVal = *Result.get<decltype(Val)>();
  EXPECT_EQ(NewVal.BlockId, Val.BlockId);
  EXPECT_EQ(NewVal.Offset, Val.Offset);
}

TEST(Unit_AuxData, nestedProtobufRoundTrip) {
  AuxData Original;
  proto::AuxData Message;
  AuxData Result;

  // Outer vector
  std::vector<std::map<char, std::tuple<int64_t, uint64_t>>> N1;
  N1.push_back({{'a', {0, 1}}, {'b', {2, 3}}});
  N1.push_back({{'c', {4, 5}}, {'d', {6, 7}}});

  Original = N1;

  Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(*Result.get<decltype(N1)>(), N1);

  // Outer map
  std::map<std::string, std::vector<int64_t>> N2{{"a", {1, 2, 3}}};
  Original = N2;
  Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(*Result.get<decltype(N2)>(), N2);

  // Outer tuple
  std::tuple<std::string, std::vector<int64_t>> N3{"a", {1, 2, 3}};
  Original = N3;
  Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(*Result.get<decltype(N3)>(), N3);
}

TEST(Unit_AuxData, wrongTypeAfterProtobufRoundTrip) {
  AuxData Original;
  Original = 1234;

  auto Message = toProtobuf(Original);
  AuxData Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(Result.get<std::string>(), nullptr);
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
  static std::string type_id() { return "MoveTest"; }
};

TEST(Unit_AuxData, movesAndCopies) {
  MoveTest::CopyCount = 0;
  MoveTest::MoveCount = 0;

  MoveTest M(123);
  EXPECT_EQ(MoveTest::CopyCount, 0);
  EXPECT_EQ(MoveTest::MoveCount, 0);

  AuxData AuxData;
  AuxData = M;
  EXPECT_EQ(AuxData.get<decltype(M)>()->Val, 123);
  EXPECT_EQ(MoveTest::CopyCount, 1);
  EXPECT_EQ(MoveTest::MoveCount, 0);

  AuxData = std::move(M);
  EXPECT_EQ(AuxData.get<decltype(M)>()->Val, 123);
  EXPECT_EQ(MoveTest::CopyCount, 1);
  EXPECT_EQ(MoveTest::MoveCount, 1);
}
