#include <gtirb/Context.hpp>
#include <gtirb/Table.hpp>
#include <proto/Table.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_Table, eaMapProtobufRoundTrip) {
  using MapT = std::map<EA, table::ValueType>;
  Table Original = MapT({{EA(1), {"a"}}, {EA(2), {"b"}}});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = std::get<MapT>(Result);
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(std::get<std::string>(M[EA(1)]), "a");
  EXPECT_EQ(std::get<std::string>(M[EA(2)]), "b");
}

TEST(Unit_Table, intMapProtobufRoundTrip) {
  using MapT = std::map<int64_t, table::ValueType>;
  Table Original = MapT({{1, {"a"}}, {2, {"b"}}});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = std::get<MapT>(Result);
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(std::get<std::string>(M[1]), "a");
  EXPECT_EQ(std::get<std::string>(M[2]), "b");
}

TEST(Unit_Table, stringMapProtobufRoundTrip) {
  using MapT = std::map<std::string, table::ValueType>;
  Table Original = MapT({{"1", {"a"}}, {"2", {"b"}}});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = std::get<MapT>(Result);
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(std::get<std::string>(M["1"]), "a");
  EXPECT_EQ(std::get<std::string>(M["2"]), "b");
}

TEST(Unit_Table, uuidMapProtobufRoundTrip) {
  using MapT = std::map<UUID, table::ValueType>;
  UUID Id1 = Node::Create(Ctx)->getUUID();
  UUID Id2 = Node::Create(Ctx)->getUUID();
  Table Original = MapT({{Id1, {"a"}}, {Id2, {"b"}}});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = std::get<MapT>(Result);
  EXPECT_EQ(M.size(), 2);
  EXPECT_EQ(std::get<std::string>(M[Id1]), "a");
  EXPECT_EQ(std::get<std::string>(M[Id2]), "b");
}

TEST(Unit_Table, mapVectorProtobufRoundTrip) {
  table::InnerMapType item({{"key", {1}}});
  Table Original = std::vector<table::InnerMapType>({item});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  auto v = std::get<std::vector<table::InnerMapType>>(Result);
  EXPECT_EQ(v.size(), 1);
  EXPECT_EQ(std::get<int64_t>(v[0]["key"]), 1);
}

TEST(Unit_Table, eaVectorProtobufRoundTrip) {
  Table Original = std::vector<EA>({EA(1), EA(2), EA(3)});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(std::get<std::vector<EA>>(Result),
            std::vector<EA>({EA(1), EA(2), EA(3)}));
}

TEST(Unit_Table, intVectorProtobufRoundTrip) {
  Table Original = std::vector<int64_t>({1, 2, 3});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(std::get<std::vector<int64_t>>(Result),
            std::vector<int64_t>({1, 2, 3}));
}

TEST(Unit_Table, stringVectorProtobufRoundTrip) {
  Table Original = std::vector<std::string>({"1", "2", "3"});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(std::get<std::vector<std::string>>(Result),
            std::vector<std::string>({"1", "2", "3"}));
}

TEST(Unit_Table, uuidVectorProtobufRoundTrip) {
  UUID Id1 = Node::Create(Ctx)->getUUID(), Id2 = Node::Create(Ctx)->getUUID(),
       Id3 = Node::Create(Ctx)->getUUID();
  Table Original = std::vector<UUID>({Id1, Id2, Id3});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  EXPECT_EQ(std::get<std::vector<UUID>>(Result),
            std::vector<UUID>({Id1, Id2, Id3}));
}

TEST(Unit_Table, instructionVectorProtobufRoundTrip) {
  UUID Id1 = Node::Create(Ctx)->getUUID(), Id2 = Node::Create(Ctx)->getUUID(),
       Id3 = Node::Create(Ctx)->getUUID();
  std::vector<InstructionRef> vec({{Id1, 1}, {Id2, 2}, {Id3, 3}});
  Table Original = vec;

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  auto vec2 = std::get<std::vector<InstructionRef>>(Result);
  EXPECT_EQ(vec2[0].BlockRef.getUUID(), vec[0].BlockRef.getUUID());
  EXPECT_EQ(vec2[0].Offset, vec[0].Offset);
  EXPECT_EQ(vec2[1].BlockRef.getUUID(), vec[1].BlockRef.getUUID());
  EXPECT_EQ(vec2[1].Offset, vec[1].Offset);
  EXPECT_EQ(vec2[2].BlockRef.getUUID(), vec[2].BlockRef.getUUID());
  EXPECT_EQ(vec2[2].Offset, vec[2].Offset);
}

TEST(Unit_Table, valueProtobufRoundTrip) {
  using MapT = std::map<int64_t, table::ValueType>;
  table::InnerMapType Inner({{"a", 1}});
  UUID Id = Node::Create(Ctx)->getUUID();
  Table Original = MapT({
      {1, EA(5)},                // EA
      {2, 6},                    // int64
      {3, "7"},                  // string
      {4, Inner},                // InnerMapType
      {5, Id},                   // UUID
      {6, InstructionRef{Id, 1}} // InstructionRef
  });

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  MapT M = std::get<MapT>(Result);
  EXPECT_EQ(M.size(), 6);
  EXPECT_EQ(std::get<EA>(M[1]), EA(5));
  EXPECT_EQ(std::get<int64_t>(M[2]), 6);
  EXPECT_EQ(std::get<std::string>(M[3]), "7");
  EXPECT_EQ(std::get<int64_t>(std::get<table::InnerMapType>(M[4])["a"]), 1);
  EXPECT_EQ(std::get<UUID>(M[5]), Id);
  auto Ref = std::get<InstructionRef>(M[6]);
  EXPECT_EQ(Ref.BlockRef.getUUID(), Id);
  EXPECT_EQ(Ref.Offset, 1);
}

TEST(Unit_Table, innerValueProtobufRoundTrip) {
  using MapT = std::map<int64_t, table::ValueType>;
  UUID Id1 = Node::Create(Ctx)->getUUID(), Id2 = Node::Create(Ctx)->getUUID();
  table::InnerMapType inner({
      {"a", EA(1)},                           // EA
      {"b", 2},                               // int
      {"c", "3"},                             // string
      {"d", std::vector<EA>({EA(4)})},        // EA vector
      {"e", std::vector<int64_t>({5})},       // int vector
      {"f", std::vector<std::string>({"6"})}, // string vector
      {"g", Id1},                             // UUID,
      {"h", std::vector<UUID>({Id2})},        // UUID vector
      {"i", InstructionRef{Id1, 1}},          // InstructionRef
      {"j", std::vector<InstructionRef>(
                {{Id1, 1}, {Id2, 2}})}, // InstructionRef vector
  });
  Table Original = MapT({{1, inner}});

  gtirb::Table Result;
  auto Message = toProtobuf(Original);
  fromProtobuf(Ctx, Result, Message);

  auto M = std::get<table::InnerMapType>(std::get<MapT>(Result)[1]);
  EXPECT_EQ(M.size(), 10);
  EXPECT_EQ(std::get<EA>(M["a"]), EA(1));
  EXPECT_EQ(std::get<int64_t>(M["b"]), 2);
  EXPECT_EQ(std::get<std::string>(M["c"]), "3");
  EXPECT_EQ(std::get<std::vector<EA>>(M["d"]), std::vector<EA>({EA(4)}));
  EXPECT_EQ(std::get<std::vector<int64_t>>(M["e"]), std::vector<int64_t>({5}));
  EXPECT_EQ(std::get<std::vector<std::string>>(M["f"]),
            std::vector<std::string>({"6"}));
  EXPECT_EQ(std::get<UUID>(M["g"]), Id1);
  EXPECT_EQ(std::get<std::vector<UUID>>(M["h"]), std::vector<UUID>({Id2}));

  auto Ref = std::get<InstructionRef>(M["i"]);
  EXPECT_EQ(Ref.BlockRef.getUUID(), Id1);
  EXPECT_EQ(Ref.Offset, 1);
  auto Refs = std::get<std::vector<InstructionRef>>(M["j"]);
  EXPECT_EQ(Refs[0].BlockRef.getUUID(), Id1);
  EXPECT_EQ(Refs[0].Offset, 1);
  EXPECT_EQ(Refs[1].BlockRef.getUUID(), Id2);
  EXPECT_EQ(Refs[1].Offset, 2);
}
