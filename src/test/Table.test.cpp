//===- Table.test.cpp -------------------------------------------*- C++ -*-===//
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
#include <gtirb/Context.hpp>
#include <gtirb/Table.hpp>
#include <proto/Table.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_Table, eaMapProtobufRoundTrip) {
  using MapT = std::map<Addr, std::string>;
  Table Original = MapT({{Addr(1), {"a"}}, {Addr(2), {"b"}}});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = get<MapT>(Result);
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(M[Addr(1)], "a");
  EXPECT_EQ(M[Addr(2)], "b");
}

TEST(Unit_Table, intMapProtobufRoundTrip) {
  using MapT = std::map<int64_t, std::string>;
  Table Original = MapT({{1, {"a"}}, {2, {"b"}}});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = get<MapT>(Result);
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(M[1], "a");
  EXPECT_EQ(M[2], "b");
}

TEST(Unit_Table, stringMapProtobufRoundTrip) {
  using MapT = std::map<std::string, std::string>;
  Table Original = MapT({{"1", {"a"}}, {"2", {"b"}}});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = get<MapT>(Result);
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(M["1"], "a");
  EXPECT_EQ(M["2"], "b");
}

TEST(Unit_Table, uuidMapProtobufRoundTrip) {
  using MapT = std::map<UUID, std::string>;
  UUID Id1 = Node::Create(Ctx)->getUUID();
  UUID Id2 = Node::Create(Ctx)->getUUID();
  Table Original = MapT({{Id1, {"a"}}, {Id2, {"b"}}});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = get<MapT>(Result);
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(M[Id1], "a");
  EXPECT_EQ(M[Id2], "b");
}

TEST(Unit_Table, mapVectorProtobufRoundTrip) {
  auto Val = std::vector<std::map<std::string, int>>{{{"key", {1}}}};

  Table Original(Val);
  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  auto New = get<decltype(Val)>(Result);
  EXPECT_EQ(New, Val);
}

TEST(Unit_Table, eaVectorProtobufRoundTrip) {
  Table Original = std::vector<Addr>({Addr(1), Addr(2), Addr(3)});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(get<std::vector<Addr>>(Result),
            std::vector<Addr>({Addr(1), Addr(2), Addr(3)}));
}

TEST(Unit_Table, intVectorProtobufRoundTrip) {
  Table Original = std::vector<int64_t>({1, 2, 3});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(get<std::vector<int64_t>>(Result), std::vector<int64_t>({1, 2, 3}));
}

TEST(Unit_Table, stringVectorProtobufRoundTrip) {
  Table Original = std::vector<std::string>({"1", "2", "3"});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(get<std::vector<std::string>>(Result),
            std::vector<std::string>({"1", "2", "3"}));
}

TEST(Unit_Table, uuidVectorProtobufRoundTrip) {
  UUID Id1 = Node::Create(Ctx)->getUUID(), Id2 = Node::Create(Ctx)->getUUID(),
       Id3 = Node::Create(Ctx)->getUUID();
  Table Original = std::vector<UUID>({Id1, Id2, Id3});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(get<std::vector<UUID>>(Result), std::vector<UUID>({Id1, Id2, Id3}));
}

TEST(Unit_Table, instructionVectorProtobufRoundTrip) {
  UUID Id1 = Node::Create(Ctx)->getUUID(), Id2 = Node::Create(Ctx)->getUUID(),
       Id3 = Node::Create(Ctx)->getUUID();
  std::vector<InstructionRef> vec({{Id1, 1}, {Id2, 2}, {Id3, 3}});
  Table Original = vec;

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  auto vec2 = get<std::vector<InstructionRef>>(Result);
  EXPECT_EQ(vec2[0].BlockRef.getUUID(), vec[0].BlockRef.getUUID());
  EXPECT_EQ(vec2[0].Offset, vec[0].Offset);
  EXPECT_EQ(vec2[1].BlockRef.getUUID(), vec[1].BlockRef.getUUID());
  EXPECT_EQ(vec2[1].Offset, vec[1].Offset);
  EXPECT_EQ(vec2[2].BlockRef.getUUID(), vec[2].BlockRef.getUUID());
  EXPECT_EQ(vec2[2].Offset, vec[2].Offset);
}

TEST(Unit_Table, typeName) {
  EXPECT_EQ(TableTemplate<uint64_t>().typeName(), "uint64_t");
  EXPECT_EQ(TableTemplate<std::vector<uint64_t>>().typeName(),
            "sequence<uint64_t>");
  std::string X = TableTemplate<std::map<int64_t, uint64_t>>().typeName();
  EXPECT_EQ(X, "mapping<int64_t,uint64_t>");

  X = TableTemplate<std::map<int64_t, std::vector<uint64_t>>>().typeName();
  EXPECT_EQ(X, "mapping<int64_t,sequence<uint64_t>>");

  X = TableTemplate<std::vector<std::map<int64_t, uint64_t>>>().typeName();
  EXPECT_EQ(X, "sequence<mapping<int64_t,uint64_t>>");

  X = TableTemplate<std::vector<std::tuple<int64_t, uint64_t>>>().typeName();
  EXPECT_EQ(X, "sequence<tuple<int64_t,uint64_t>>");
}

TEST(Unit_Table, getPrimitiveTypes) {
  Table P;
  P = char('a');
  EXPECT_EQ(get<char>(P), 'a');

  P = uint64_t(123);
  EXPECT_EQ(get<uint64_t>(P), 123);

  P = int64_t(-123);
  EXPECT_EQ(get<int64_t>(P), -123);

  P = uint8_t(123);
  EXPECT_EQ(get<uint8_t>(P), 123);

  P = int(-123);
  EXPECT_EQ(get<int>(P), -123);

  P = unsigned(123);
  EXPECT_EQ(get<unsigned>(P), 123);

  P = std::byte(123);
  EXPECT_EQ(get<std::byte>(P), std::byte(123));
}

TEST(Unit_Table, getVector) {
  std::vector<int64_t> Orig({1, 2, 3});
  Table P(Orig);

  auto& result = get<std::vector<int64_t>>(P);
  EXPECT_EQ(result, Orig);
}

TEST(Unit_Table, getString) {
  std::string Orig("abcd");
  Table P(Orig);

  EXPECT_EQ(get<std::string>(P), "abcd");
}

TEST(Unit_Table, getAddr) {
  Addr Orig(0x1234);
  Table P(Orig);

  EXPECT_EQ(get<Addr>(P), Addr(0x1234));
}

TEST(Unit_Table, getMap) {
  std::map<char, int64_t> Orig({{'a', 1}, {'b', 2}, {'c', 3}});
  Table P(Orig);

  auto& Result = get<std::map<char, int64_t>>(P);
  EXPECT_EQ(Result, Orig);
}

TEST(Unit_Table, getTuple) {
  std::tuple<char, int64_t> Orig('a', 1);
  Table P(Orig);

  auto& Result = get<std::tuple<char, int64_t>>(P);
  EXPECT_EQ(Result, Orig);
}

TEST(Unit_Table, getUnsetValue) {
  Table P;
  EXPECT_DEATH(get<int64_t>(P), "");
}

TEST(Unit_Table, getWrongType) {
  Table P;
  P = uint64_t(1);
  EXPECT_DEATH(get<int64_t>(P), "");
}

TEST(Unit_Table, getWrongContainer) {
  Table P;
  P = std::vector<int>();
  EXPECT_DEATH(get<std::list<int>>(P), "");
}

TEST(Unit_Table, protobufRoundTrip) {
  int64_t A = 123;
  Table Original;
  Original = A;

  auto Message = toProtobuf(Original);
  Table Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(A, get<int64_t>(Result));
}

TEST(Unit_Table, vectorProtobufRoundTrip) {
  std::vector<int64_t> V({1, 2, 3});
  Table Original;
  Original = V;

  auto Message = toProtobuf(Original);
  Table Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(get<decltype(V)>(Result), V);
}

TEST(Unit_Table, listProtobufRoundTrip) {
  std::list<int64_t> V({1, 2, 3});
  Table Original;
  Original = V;

  auto Message = toProtobuf(Original);
  Table Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(get<decltype(V)>(Result), V);
}

TEST(Unit_Table, listToVectorProtobufRoundTrip) {
  std::list<int64_t> Lst({1, 2, 3});
  Table Original;
  Original = Lst;

  auto Message = toProtobuf(Original);
  Table Result;
  fromProtobuf(Ctx, Result, Message);

  auto vec = std::vector<int64_t>(Lst.begin(), Lst.end());
  EXPECT_EQ(get<decltype(vec)>(Result), vec);
}

TEST(Unit_Table, stringProtobufRoundTrip) {
  std::string S("abcd");
  Table Original;
  Original = S;

  auto Message = toProtobuf(Original);
  Table Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(get<decltype(S)>(Result), S);
}

TEST(Unit_Table, addrProtobufRoundTrip) {
  Addr A(0x1234);
  Table Original;
  Original = A;

  auto Message = toProtobuf(Original);
  Table Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(get<decltype(A)>(Result), A);
}

TEST(Unit_Table, mapProtobufRoundTrip) {
  std::map<char, int64_t> M({{'a', 1}, {'b', 2}, {'c', 3}});
  Table Original;
  Original = M;

  auto Message = toProtobuf(Original);
  Table Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(get<decltype(M)>(Result), M);
}

TEST(Unit_Table, tupleProtobufRoundTrip) {
  std::tuple<char, int64_t> T('a', 1);
  Table Original;
  Original = T;

  auto Message = toProtobuf(Original);
  Table Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(get<decltype(T)>(Result), T);
}

TEST(Unit_Table, uuidProtobufRoundTrip) {
  UUID Val = Node::Create(Ctx)->getUUID();
  Table Original;
  Original = Val;

  auto Message = toProtobuf(Original);
  Table Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(get<decltype(Val)>(Result), Val);
}

TEST(Unit_Table, instructionRefProtobufRoundTrip) {
  InstructionRef Val{{Node::Create(Ctx)->getUUID()}, 123};
  Table Original;
  Original = Val;

  auto Message = toProtobuf(Original);
  Table Result;
  fromProtobuf(Ctx, Result, Message);

  auto NewVal = get<decltype(Val)>(Result);
  EXPECT_EQ(NewVal.BlockRef.getUUID(), Val.BlockRef.getUUID());
  EXPECT_EQ(NewVal.Offset, Val.Offset);
}

TEST(Unit_Table, nestedProtobufRoundTrip) {
  Table Original;
  proto::Table Message;
  Table Result;

  // Outer vector
  std::vector<std::map<char, std::tuple<int64_t, uint64_t>>> N1;
  N1.push_back({{'a', {0, 1}}, {'b', {2, 3}}});
  N1.push_back({{'c', {4, 5}}, {'d', {6, 7}}});

  Original = N1;

  Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(get<decltype(N1)>(Result), N1);

  // Outer map
  std::map<std::string, std::vector<int64_t>> N2{{"a", {1, 2, 3}}};
  Original = N2;
  Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(get<decltype(N2)>(Result), N2);
}

TEST(Unit_Table, wrongTypeAfterProtobufRoundTrip) {
  Table Original;
  Original = 1234;

  auto Message = toProtobuf(Original);
  Table Result;
  fromProtobuf(Ctx, Result, Message);

  EXPECT_DEATH(get<std::string>(Result), "");
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
struct gtirb::table_traits<MoveTest> : default_serialization<MoveTest> {
  static std::string type_id() { return "MoveTest"; }
};

TEST(Unit_Table, movesAndCopies) {
  MoveTest::CopyCount = 0;
  MoveTest::MoveCount = 0;

  MoveTest M(123);
  EXPECT_EQ(MoveTest::CopyCount, 0);
  EXPECT_EQ(MoveTest::MoveCount, 0);

  Table Table;
  Table = M;
  EXPECT_EQ(get<decltype(M)>(Table).Val, 123);
  EXPECT_EQ(MoveTest::CopyCount, 1);
  EXPECT_EQ(MoveTest::MoveCount, 0);

  Table = std::move(M);
  EXPECT_EQ(get<decltype(M)>(Table).Val, 123);
  EXPECT_EQ(MoveTest::CopyCount, 1);
  EXPECT_EQ(MoveTest::MoveCount, 1);
}

TEST(Unit_Table, getIf) {
  Table T(int(123));
  EXPECT_EQ(*get_if<int>(&T), 123);
  EXPECT_EQ(get_if<std::string>(&T), nullptr);

  Table Empty;
  EXPECT_EQ(get_if<int>(&Empty), nullptr);
  EXPECT_EQ(get_if<int>(nullptr), nullptr);
}
