#include <gtirb/Table.hpp>
#include <proto/Table.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

TEST(Unit_Table, eaMapProtobufRoundTrip) {
  using MapT = std::map<EA, table::ValueType>;
  Table original = MapT({{EA(1), {"a"}}, {EA(2), {"b"}}});

  gtirb::Table result;
  auto message = toProtobuf(original);
  fromProtobuf(result, message);

  MapT m = boost::get<MapT>(result);
  EXPECT_EQ(m.size(), 2);
  EXPECT_EQ(boost::get<std::string>(m[EA(1)]), "a");
  EXPECT_EQ(boost::get<std::string>(m[EA(2)]), "b");
}

TEST(Unit_Table, intMapProtobufRoundTrip) {
  using MapT = std::map<int64_t, table::ValueType>;
  Table original = MapT({{1, {"a"}}, {2, {"b"}}});

  gtirb::Table result;
  auto message = toProtobuf(original);
  fromProtobuf(result, message);

  MapT m = boost::get<MapT>(result);
  EXPECT_EQ(m.size(), 2);
  EXPECT_EQ(boost::get<std::string>(m[1]), "a");
  EXPECT_EQ(boost::get<std::string>(m[2]), "b");
}

TEST(Unit_Table, stringMapProtobufRoundTrip) {
  using MapT = std::map<std::string, table::ValueType>;
  Table original = MapT({{"1", {"a"}}, {"2", {"b"}}});

  gtirb::Table result;
  auto message = toProtobuf(original);
  fromProtobuf(result, message);

  MapT m = boost::get<MapT>(result);
  EXPECT_EQ(m.size(), 2);
  EXPECT_EQ(boost::get<std::string>(m["1"]), "a");
  EXPECT_EQ(boost::get<std::string>(m["2"]), "b");
}

TEST(Unit_Table, uuidMapProtobufRoundTrip) {
  using MapT = std::map<UUID, table::ValueType>;
  UUID id1 = Node().getUUID();
  UUID id2 = Node().getUUID();
  Table original = MapT({{id1, {"a"}}, {id2, {"b"}}});

  gtirb::Table result;
  auto message = toProtobuf(original);
  fromProtobuf(result, message);

  MapT m = boost::get<MapT>(result);
  EXPECT_EQ(m.size(), 2);
  EXPECT_EQ(boost::get<std::string>(m[id1]), "a");
  EXPECT_EQ(boost::get<std::string>(m[id2]), "b");
}

TEST(Unit_Table, mapVectorProtobufRoundTrip) {
  table::InnerMapType item({{"key", {1}}});
  Table original = std::vector<table::InnerMapType>({item});

  gtirb::Table result;
  auto message = toProtobuf(original);
  fromProtobuf(result, message);

  auto v = boost::get<std::vector<table::InnerMapType>>(result);
  EXPECT_EQ(v.size(), 1);
  EXPECT_EQ(boost::get<int64_t>(v[0]["key"]), 1);
}

TEST(Unit_Table, eaVectorProtobufRoundTrip) {
  Table original = std::vector<EA>({EA(1), EA(2), EA(3)});

  gtirb::Table result;
  auto message = toProtobuf(original);
  fromProtobuf(result, message);

  EXPECT_EQ(boost::get<std::vector<EA>>(result), std::vector<EA>({EA(1), EA(2), EA(3)}));
}

TEST(Unit_Table, intVectorProtobufRoundTrip) {
  Table original = std::vector<int64_t>({1, 2, 3});

  gtirb::Table result;
  auto message = toProtobuf(original);
  fromProtobuf(result, message);

  EXPECT_EQ(boost::get<std::vector<int64_t>>(result), std::vector<int64_t>({1, 2, 3}));
}

TEST(Unit_Table, stringVectorProtobufRoundTrip) {
  Table original = std::vector<std::string>({"1", "2", "3"});

  gtirb::Table result;
  auto message = toProtobuf(original);
  fromProtobuf(result, message);

  EXPECT_EQ(boost::get<std::vector<std::string>>(result),
            std::vector<std::string>({"1", "2", "3"}));
}

TEST(Unit_Table, uuidVectorProtobufRoundTrip) {
  UUID id1 = Node().getUUID(), id2 = Node().getUUID(), id3 = Node().getUUID();
  Table original = std::vector<UUID>({id1, id2, id3});

  gtirb::Table result;
  auto message = toProtobuf(original);
  fromProtobuf(result, message);

  EXPECT_EQ(boost::get<std::vector<UUID>>(result), std::vector<UUID>({id1, id2, id3}));
}

TEST(Unit_Table, instructionVectorProtobufRoundTrip) {
  UUID id1 = Node().getUUID(), id2 = Node().getUUID(), id3 = Node().getUUID();
  std::vector<InstructionRef> vec({{id1, 1}, {id2, 2}, {id3, 3}});
  Table original = vec;

  gtirb::Table result;
  auto message = toProtobuf(original);
  fromProtobuf(result, message);

  auto vec2 = boost::get<std::vector<InstructionRef>>(result);
  EXPECT_EQ(vec2[0].block.getUUID(), vec[0].block.getUUID());
  EXPECT_EQ(vec2[0].offset, vec[0].offset);
  EXPECT_EQ(vec2[1].block.getUUID(), vec[1].block.getUUID());
  EXPECT_EQ(vec2[1].offset, vec[1].offset);
  EXPECT_EQ(vec2[2].block.getUUID(), vec[2].block.getUUID());
  EXPECT_EQ(vec2[2].offset, vec[2].offset);
}

TEST(Unit_Table, valueProtobufRoundTrip) {
  using MapT = std::map<int64_t, table::ValueType>;
  table::InnerMapType inner({{"a", 1}});
  UUID id = Node().getUUID();
  Table original = MapT({
      {1, EA(5)},                // EA
      {2, 6},                    // int64
      {3, "7"},                  // string
      {4, inner},                // InnerMapType
      {5, id},                   // UUID
      {6, InstructionRef{id, 1}} // InstructionRef
  });

  gtirb::Table result;
  auto message = toProtobuf(original);
  fromProtobuf(result, message);

  MapT m = boost::get<MapT>(result);
  EXPECT_EQ(m.size(), 6);
  EXPECT_EQ(boost::get<EA>(m[1]), EA(5));
  EXPECT_EQ(boost::get<int64_t>(m[2]), 6);
  EXPECT_EQ(boost::get<std::string>(m[3]), "7");
  EXPECT_EQ(boost::get<int64_t>(boost::get<table::InnerMapType>(m[4])["a"]), 1);
  EXPECT_EQ(boost::get<UUID>(m[5]), id);
  auto ref = boost::get<InstructionRef>(m[6]);
  EXPECT_EQ(ref.block.getUUID(), id);
  EXPECT_EQ(ref.offset, 1);
}

TEST(Unit_Table, innerValueProtobufRoundTrip) {
  using MapT = std::map<int64_t, table::ValueType>;
  UUID id1 = Node().getUUID(), id2 = Node().getUUID();
  table::InnerMapType inner({
      {"a", EA(1)},                                             // EA
      {"b", 2},                                                 // int
      {"c", "3"},                                               // string
      {"d", std::vector<EA>({EA(4)})},                          // EA vector
      {"e", std::vector<int64_t>({5})},                         // int vector
      {"f", std::vector<std::string>({"6"})},                   // string vector
      {"g", id1},                                               // UUID,
      {"h", std::vector<UUID>({id2})},                          // UUID vector
      {"i", InstructionRef{id1, 1}},                            // InstructionRef
      {"j", std::vector<InstructionRef>({{id1, 1}, {id2, 2}})}, // InstructionRef vector
  });
  Table original = MapT({{1, inner}});

  gtirb::Table result;
  auto message = toProtobuf(original);
  fromProtobuf(result, message);

  auto m = boost::get<table::InnerMapType>(boost::get<MapT>(result)[1]);
  EXPECT_EQ(m.size(), 10);
  EXPECT_EQ(boost::get<EA>(m["a"]), EA(1));
  EXPECT_EQ(boost::get<int64_t>(m["b"]), 2);
  EXPECT_EQ(boost::get<std::string>(m["c"]), "3");
  EXPECT_EQ(boost::get<std::vector<EA>>(m["d"]), std::vector<EA>({EA(4)}));
  EXPECT_EQ(boost::get<std::vector<int64_t>>(m["e"]), std::vector<int64_t>({5}));
  EXPECT_EQ(boost::get<std::vector<std::string>>(m["f"]), std::vector<std::string>({"6"}));
  EXPECT_EQ(boost::get<UUID>(m["g"]), id1);
  EXPECT_EQ(boost::get<std::vector<UUID>>(m["h"]), std::vector<UUID>({id2}));

  auto ref = boost::get<InstructionRef>(m["i"]);
  EXPECT_EQ(ref.block.getUUID(), id1);
  EXPECT_EQ(ref.offset, 1);

  auto refs = boost::get<std::vector<InstructionRef>>(m["j"]);
  EXPECT_EQ(refs[0].block.getUUID(), id1);
  EXPECT_EQ(refs[0].offset, 1);
  EXPECT_EQ(refs[1].block.getUUID(), id2);
  EXPECT_EQ(refs[1].offset, 2);
}
