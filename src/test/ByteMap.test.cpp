#include <gtirb/ByteMap.hpp>
#include <proto/ByteMap.pb.h>
#include <gsl/span>
#include <gtest/gtest.h>
#include <memory>

using namespace gtirb;

TEST(Unit_ByteMap, newRegion) {
  ByteMap byteMap;
  std::vector<gsl::byte> data = {gsl::byte(1), gsl::byte(2), gsl::byte(3),
                                 gsl::byte(4)};

  byteMap.setData(EA(1000), as_bytes(gsl::make_span(data)));

  EXPECT_EQ(byteMap.getData(EA(1000), data.size()), data);
  EXPECT_THROW(byteMap.getData(EA(999), data.size()), std::out_of_range);
  EXPECT_THROW(byteMap.getData(EA(1000), data.size() + 1), std::out_of_range);
}

TEST(Unit_ByteMap, secondRegionAfter) {
  ByteMap byteMap;
  std::vector<gsl::byte> data1 = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};
  std::vector<gsl::byte> data2 = {gsl::byte(4), gsl::byte(5), gsl::byte(6)};

  byteMap.setData(EA(1000), as_bytes(gsl::make_span(data1)));
  byteMap.setData(EA(2000), as_bytes(gsl::make_span(data2)));

  EXPECT_EQ(byteMap.getData(EA(1000), data1.size()), data1);
  EXPECT_EQ(byteMap.getData(EA(2000), data2.size()), data2);
  EXPECT_THROW(byteMap.getData(EA(999), data1.size()), std::out_of_range);
  EXPECT_THROW(byteMap.getData(EA(1000), data1.size() + 1), std::out_of_range);
  EXPECT_THROW(byteMap.getData(EA(1999), data2.size()), std::out_of_range);
  EXPECT_THROW(byteMap.getData(EA(2000), data2.size() + 1), std::out_of_range);
}

TEST(Unit_ByteMap, secondRegionBefore) {
  ByteMap byteMap;
  std::vector<gsl::byte> data1 = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};
  std::vector<gsl::byte> data2 = {gsl::byte(4), gsl::byte(5), gsl::byte(6)};

  byteMap.setData(EA(2000), as_bytes(gsl::make_span(data2)));
  // Leave one byte in between
  byteMap.setData(EA(1996), as_bytes(gsl::make_span(data1)));

  EXPECT_EQ(byteMap.getData(EA(1996), data1.size()), data1);
  EXPECT_EQ(byteMap.getData(EA(2000), data2.size()), data2);
  EXPECT_THROW(byteMap.getData(EA(999), data1.size()), std::out_of_range);
  EXPECT_THROW(byteMap.getData(EA(1996), data1.size() + 1), std::out_of_range);
  EXPECT_THROW(byteMap.getData(EA(1999), data2.size()), std::out_of_range);
  EXPECT_THROW(byteMap.getData(EA(2000), data2.size() + 1), std::out_of_range);
}

TEST(Unit_ByteMap, thirdRegionBetween) {
  ByteMap byteMap;
  std::vector<gsl::byte> data1 = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};
  std::vector<gsl::byte> data2 = {gsl::byte(4), gsl::byte(5), gsl::byte(6)};
  std::vector<gsl::byte> data3 = {gsl::byte(7), gsl::byte(8), gsl::byte(9)};

  byteMap.setData(EA(1000), as_bytes(gsl::make_span(data1)));
  byteMap.setData(EA(2000), as_bytes(gsl::make_span(data2)));
  byteMap.setData(EA(1100), as_bytes(gsl::make_span(data3)));

  EXPECT_EQ(byteMap.getData(EA(1000), data1.size()), data1);
  EXPECT_EQ(byteMap.getData(EA(2000), data2.size()), data2);
  EXPECT_EQ(byteMap.getData(EA(1100), data3.size()), data3);
  EXPECT_THROW(byteMap.getData(EA(1199), data3.size()), std::out_of_range);
  EXPECT_THROW(byteMap.getData(EA(1100), data3.size() + 1), std::out_of_range);
  EXPECT_THROW(byteMap.getData(EA(1105), 1), std::out_of_range);
}

TEST(Unit_ByteMap, extendRegionUp) {
  ByteMap byteMap;
  std::vector<gsl::byte> data1 = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};
  std::vector<gsl::byte> data2 = {gsl::byte(4), gsl::byte(5), gsl::byte(6)};

  byteMap.setData(EA(1000), as_bytes(gsl::make_span(data1)));
  byteMap.setData(EA(1000 + data1.size()), as_bytes(gsl::make_span(data2)));

  auto expected = data1;
  expected.insert(expected.end(), data2.begin(), data2.end());
  EXPECT_EQ(byteMap.getData(EA(1000), expected.size()), expected);
}

TEST(Unit_ByteMap, extendRegionDown) {
  ByteMap byteMap;
  std::vector<gsl::byte> data1 = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};
  std::vector<gsl::byte> data2 = {gsl::byte(4), gsl::byte(5), gsl::byte(6)};

  byteMap.setData(EA(1000 + data1.size()), as_bytes(gsl::make_span(data2)));
  byteMap.setData(EA(1000), as_bytes(gsl::make_span(data1)));

  auto expected = data1;
  expected.insert(expected.end(), data2.begin(), data2.end());
  EXPECT_EQ(byteMap.getData(EA(1000), expected.size()), expected);
}

TEST(Unit_ByteMap, mergeRegions) {
  ByteMap byteMap;
  std::vector<gsl::byte> data1 = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};
  std::vector<gsl::byte> data2 = {gsl::byte(4), gsl::byte(5), gsl::byte(6)};
  std::vector<gsl::byte> data3 = {gsl::byte(4), gsl::byte(5), gsl::byte(6)};

  byteMap.setData(EA(1000 + data1.size() + data2.size()),
                  as_bytes(gsl::make_span(data3)));
  byteMap.setData(EA(1000), as_bytes(gsl::make_span(data1)));
  byteMap.setData(EA(1000 + data1.size()), as_bytes(gsl::make_span(data2)));

  auto expected = data1;
  expected.insert(expected.end(), data2.begin(), data2.end());
  expected.insert(expected.end(), data3.begin(), data3.end());
  EXPECT_EQ(byteMap.getData(EA(1000), expected.size()), expected);
}

TEST(Unit_ByteMap, overwriteExistingData) {
  ByteMap byteMap;
  std::vector<gsl::byte> data1 = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};
  std::vector<gsl::byte> data2 = {gsl::byte(4), gsl::byte(5)};

  byteMap.setData(EA(1000), as_bytes(gsl::make_span(data1)));
  byteMap.setData(EA(1000), as_bytes(gsl::make_span(data2)));

  std::vector<gsl::byte> expected1 = {gsl::byte(4), gsl::byte(5), gsl::byte(3)};
  EXPECT_EQ(byteMap.getData(EA(1000), expected1.size()), expected1);

  byteMap.setData(EA(1001), as_bytes(gsl::make_span(data2)));

  std::vector<gsl::byte> expected2 = {gsl::byte(4), gsl::byte(4), gsl::byte(5)};
  EXPECT_EQ(byteMap.getData(EA(1000), expected2.size()), expected2);
}

TEST(Unit_ByteMap, overlappingRegionsAreInvalid) {
  ByteMap byteMap;
  std::vector<gsl::byte> data1 = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};
  std::vector<gsl::byte> data2 = {gsl::byte(4), gsl::byte(5), gsl::byte(6)};
  std::vector<gsl::byte> big(1000);

  byteMap.setData(EA(1000), as_bytes(gsl::make_span(data1)));
  byteMap.setData(EA(2000), as_bytes(gsl::make_span(data2)));

  EXPECT_THROW(byteMap.setData(EA(999), as_bytes(gsl::make_span(data1))),
               std::invalid_argument);
  EXPECT_THROW(byteMap.setData(EA(1001), as_bytes(gsl::make_span(data1))),
               std::invalid_argument);
  EXPECT_THROW(byteMap.setData(EA(1999), as_bytes(gsl::make_span(data1))),
               std::invalid_argument);
  EXPECT_THROW(byteMap.setData(EA(2001), as_bytes(gsl::make_span(data1))),
               std::invalid_argument);
  EXPECT_THROW(
      byteMap.setData(EA(1000) + data1.size(), as_bytes(gsl::make_span(big))),
      std::invalid_argument);
}

TEST(Unit_ByteMap, getDataUnmapped) {
  ByteMap byteMap;
  std::vector<gsl::byte> data = {gsl::byte(1), gsl::byte(2), gsl::byte(3)};

  byteMap.setData(EA(1000), as_bytes(gsl::make_span(data)));

  EXPECT_THROW(byteMap.getData(EA(900), 3), std::out_of_range);
  EXPECT_THROW(byteMap.getData(EA(999), 3), std::out_of_range);
  EXPECT_THROW(byteMap.getData(EA(999), 10), std::out_of_range);
  EXPECT_THROW(byteMap.getData(EA(1000), data.size() + 1), std::out_of_range);
  EXPECT_THROW(byteMap.getData(EA(1000 + data.size()), 1), std::out_of_range);
}

TEST(Unit_ByteMap, protobufRoundTrip) {
  ByteMap original;
  auto a = gsl::byte('a');
  auto b = gsl::byte('b');
  auto c = gsl::byte('c');
  original.setData(EA(1), gsl::make_span(&a, 1));
  original.setData(EA(2), gsl::make_span(&b, 1));
  original.setData(EA(5000), gsl::make_span(&c, 1));

  gtirb::ByteMap result;
  proto::ByteMap message;
  original.toProtobuf(&message);
  result.fromProtobuf(message);

  EXPECT_EQ(result.getData(EA(1), 1)[0], gsl::byte('a'));
  EXPECT_EQ(result.getData(EA(2), 1)[0], gsl::byte('b'));
  EXPECT_EQ(result.getData(EA(5000), 1)[0], gsl::byte('c'));
}
