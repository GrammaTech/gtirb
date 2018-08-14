#include <gtirb/ByteMap.hpp>
#include <proto/ByteMap.pb.h>
#include <gsl/span>
#include <gtest/gtest.h>
#include <memory>

using namespace gtirb;

TEST(Unit_ByteMap, newRegion) {
  ByteMap B;
  std::vector<gsl::byte> data = {gsl::byte(1), gsl::byte(2), gsl::byte(3),
                                 gsl::byte(4)};

  B.setData(EA(1000), as_bytes(gsl::make_span(data)));

  EXPECT_EQ(B.getData(EA(1000), data.size()), data);
  EXPECT_THROW(B.getData(EA(999), data.size()), std::out_of_range);
  EXPECT_THROW(B.getData(EA(1000), data.size() + 1), std::out_of_range);
}

TEST(Unit_ByteMap, secondRegionAfter) {
  ByteMap B;
  std::vector<gsl::byte> Data1 = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};
  std::vector<gsl::byte> Data2 = {gsl::byte(4), gsl::byte(5), gsl::byte(6)};
  B.setData(EA(1000), as_bytes(gsl::make_span(Data1)));
  B.setData(EA(2000), as_bytes(gsl::make_span(Data2)));

  EXPECT_EQ(B.getData(EA(1000), Data1.size()), Data1);
  EXPECT_EQ(B.getData(EA(2000), Data2.size()), Data2);
  EXPECT_THROW(B.getData(EA(999), Data1.size()), std::out_of_range);
  EXPECT_THROW(B.getData(EA(1000), Data1.size() + 1), std::out_of_range);
  EXPECT_THROW(B.getData(EA(1999), Data2.size()), std::out_of_range);
  EXPECT_THROW(B.getData(EA(2000), Data2.size() + 1), std::out_of_range);
}

TEST(Unit_ByteMap, secondRegionBefore) {
  ByteMap B;
  std::vector<gsl::byte> Data1 = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};
  std::vector<gsl::byte> Data2 = {gsl::byte(4), gsl::byte(5), gsl::byte(6)};

  B.setData(EA(2000), as_bytes(gsl::make_span(Data2)));
  // Leave one byte in between
  B.setData(EA(1996), as_bytes(gsl::make_span(Data1)));

  EXPECT_EQ(B.getData(EA(1996), Data1.size()), Data1);
  EXPECT_EQ(B.getData(EA(2000), Data2.size()), Data2);
  EXPECT_THROW(B.getData(EA(999), Data1.size()), std::out_of_range);
  EXPECT_THROW(B.getData(EA(1996), Data1.size() + 1), std::out_of_range);
  EXPECT_THROW(B.getData(EA(1999), Data2.size()), std::out_of_range);
  EXPECT_THROW(B.getData(EA(2000), Data2.size() + 1), std::out_of_range);
}

TEST(Unit_ByteMap, thirdRegionBetween) {
  ByteMap B;
  std::vector<gsl::byte> Data1 = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};
  std::vector<gsl::byte> Data2 = {gsl::byte(4), gsl::byte(5), gsl::byte(6)};
  std::vector<gsl::byte> Data3 = {gsl::byte(7), gsl::byte(8), gsl::byte(9)};

  B.setData(EA(1000), as_bytes(gsl::make_span(Data1)));
  B.setData(EA(2000), as_bytes(gsl::make_span(Data2)));
  B.setData(EA(1100), as_bytes(gsl::make_span(Data3)));

  EXPECT_EQ(B.getData(EA(1000), Data1.size()), Data1);
  EXPECT_EQ(B.getData(EA(2000), Data2.size()), Data2);
  EXPECT_EQ(B.getData(EA(1100), Data3.size()), Data3);
  EXPECT_THROW(B.getData(EA(1199), Data3.size()), std::out_of_range);
  EXPECT_THROW(B.getData(EA(1100), Data3.size() + 1), std::out_of_range);
  EXPECT_THROW(B.getData(EA(1105), 1), std::out_of_range);
}

TEST(Unit_ByteMap, extendRegionUp) {
  ByteMap B;
  std::vector<gsl::byte> Data1 = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};
  std::vector<gsl::byte> Data2 = {gsl::byte(4), gsl::byte(5), gsl::byte(6)};

  B.setData(EA(1000), as_bytes(gsl::make_span(Data1)));
  B.setData(EA(1000 + Data1.size()), as_bytes(gsl::make_span(Data2)));

  auto Expected = Data1;
  Expected.insert(Expected.end(), Data2.begin(), Data2.end());
  EXPECT_EQ(B.getData(EA(1000), Expected.size()), Expected);
}

TEST(Unit_ByteMap, extendRegionDown) {
  ByteMap B;
  std::vector<gsl::byte> Data1 = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};
  std::vector<gsl::byte> Data2 = {gsl::byte(4), gsl::byte(5), gsl::byte(6)};

  B.setData(EA(1000 + Data1.size()), as_bytes(gsl::make_span(Data2)));
  B.setData(EA(1000), as_bytes(gsl::make_span(Data1)));

  auto Expected = Data1;
  Expected.insert(Expected.end(), Data2.begin(), Data2.end());
  EXPECT_EQ(B.getData(EA(1000), Expected.size()), Expected);
}

TEST(Unit_ByteMap, mergeRegions) {
  ByteMap B;
  std::vector<gsl::byte> Data1 = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};
  std::vector<gsl::byte> Data2 = {gsl::byte(4), gsl::byte(5), gsl::byte(6)};
  std::vector<gsl::byte> Data3 = {gsl::byte(4), gsl::byte(5), gsl::byte(6)};

  B.setData(EA(1000 + Data1.size() + Data2.size()),
            as_bytes(gsl::make_span(Data3)));
  B.setData(EA(1000), as_bytes(gsl::make_span(Data1)));
  B.setData(EA(1000 + Data1.size()), as_bytes(gsl::make_span(Data2)));

  auto Expected = Data1;
  Expected.insert(Expected.end(), Data2.begin(), Data2.end());
  Expected.insert(Expected.end(), Data3.begin(), Data3.end());
  EXPECT_EQ(B.getData(EA(1000), Expected.size()), Expected);
}

TEST(Unit_ByteMap, overwriteExistingData) {
  ByteMap B;
  std::vector<gsl::byte> Data1 = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};
  std::vector<gsl::byte> Data2 = {gsl::byte(4), gsl::byte(5)};

  B.setData(EA(1000), as_bytes(gsl::make_span(Data1)));
  B.setData(EA(1000), as_bytes(gsl::make_span(Data2)));

  std::vector<gsl::byte> Expected1 = {gsl::byte(4), gsl::byte(5), gsl::byte(3)};
  EXPECT_EQ(B.getData(EA(1000), Expected1.size()), Expected1);

  B.setData(EA(1001), as_bytes(gsl::make_span(Data2)));

  std::vector<gsl::byte> Expected2 = {gsl::byte(4), gsl::byte(4), gsl::byte(5)};
  EXPECT_EQ(B.getData(EA(1000), Expected2.size()), Expected2);
}

TEST(Unit_ByteMap, overlappingRegionsAreInvalid) {
  ByteMap B;
  std::vector<gsl::byte> Data1 = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};
  std::vector<gsl::byte> Data2 = {gsl::byte(4), gsl::byte(5), gsl::byte(6)};
  std::vector<gsl::byte> Big(1000);

  B.setData(EA(1000), as_bytes(gsl::make_span(Data1)));
  B.setData(EA(2000), as_bytes(gsl::make_span(Data2)));

  EXPECT_THROW(B.setData(EA(999), as_bytes(gsl::make_span(Data1))),
               std::invalid_argument);
  EXPECT_THROW(B.setData(EA(1001), as_bytes(gsl::make_span(Data1))),
               std::invalid_argument);
  EXPECT_THROW(B.setData(EA(1999), as_bytes(gsl::make_span(Data1))),
               std::invalid_argument);
  EXPECT_THROW(B.setData(EA(2001), as_bytes(gsl::make_span(Data1))),
               std::invalid_argument);
  EXPECT_THROW(
      B.setData(EA(1000) + Data1.size(), as_bytes(gsl::make_span(Big))),
      std::invalid_argument);
}

TEST(Unit_ByteMap, getDataUnmapped) {
  ByteMap B;
  std::vector<gsl::byte> Data = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};

  B.setData(EA(1000), as_bytes(gsl::make_span(Data)));

  EXPECT_THROW(B.getData(EA(900), 3), std::out_of_range);
  EXPECT_THROW(B.getData(EA(999), 3), std::out_of_range);
  EXPECT_THROW(B.getData(EA(999), 10), std::out_of_range);
  EXPECT_THROW(B.getData(EA(1000), Data.size() + 1), std::out_of_range);
  EXPECT_THROW(B.getData(EA(1000 + Data.size()), 1), std::out_of_range);
}

TEST(Unit_ByteMap, protobufRoundTrip) {
  ByteMap Original;
  auto a = gsl::byte('a');
  auto b = gsl::byte('b');
  auto c = gsl::byte('c');
  Original.setData(EA(1), gsl::make_span(&a, 1));
  Original.setData(EA(2), gsl::make_span(&b, 1));
  Original.setData(EA(5000), gsl::make_span(&c, 1));

  gtirb::ByteMap Result;
  proto::ByteMap Message;
  Original.toProtobuf(&Message);
  Result.fromProtobuf(Message);

  EXPECT_EQ(Result.getData(EA(1), 1)[0], gsl::byte('a'));
  EXPECT_EQ(Result.getData(EA(2), 1)[0], gsl::byte('b'));
  EXPECT_EQ(Result.getData(EA(5000), 1)[0], gsl::byte('c'));
}
