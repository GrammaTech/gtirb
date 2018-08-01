#include <gtirb/ByteMap.hpp>
#include <gtirb/Utilities.hpp>
#include <proto/ByteMap.pb.h>
#include <gsl/span>
#include <gtest/gtest.h>
#include <memory>

using namespace gtirb;

class Unit_ByteMapF : public ::testing::Test {
public:
  virtual void SetUp() override {
    for (size_t i = 0; i < this->InitializedSize; ++i) {
      auto x = uint8_t(this->InitialByte & i);
      this->byteMap.setData(Unit_ByteMapF::Offset + gtirb::EA{i}, as_bytes(gsl::make_span(&x, 1)));
    }
  }

  static constexpr uint8_t InitialByte{0xFF};
  static gtirb::EA Offset;
  static size_t InitializedSize;

  gtirb::ByteMap byteMap{};
};

gtirb::EA Unit_ByteMapF::Offset{static_cast<uint64_t>(PageSize)};
size_t Unit_ByteMapF::InitializedSize{PageSize * 2};

TEST(Unit_ByteMap, ctor_0) { EXPECT_NO_THROW(gtirb::ByteMap()); }

TEST(Unit_ByteMap, empty_0) {
  auto byteMap = gtirb::ByteMap();
  EXPECT_TRUE(byteMap.empty());
}

TEST(Unit_ByteMap, size_0) {
  auto byteMap = gtirb::ByteMap();
  EXPECT_EQ(size_t{0}, byteMap.size());
}

TEST_F(Unit_ByteMapF, empty) { EXPECT_FALSE(this->byteMap.empty()); }

TEST_F(Unit_ByteMapF, size) { EXPECT_EQ(this->InitializedSize, this->byteMap.size()); }

TEST_F(Unit_ByteMapF, legacy_boundariesWithZero_0) {
  // Boundaries w/ 0's.
  const auto ea = Unit_ByteMapF::Offset - gtirb::EA{16};
  const auto buf = this->byteMap.getData(ea, 32);

  for (size_t i = 0; i < 32; ++i) {
    if (i < 16) {
      EXPECT_EQ(0, buf[i]) << "Bad chunk read at : " << ea << " plus : " << i;
    } else {
      EXPECT_EQ((this->InitialByte & (i - 16)), buf[i])
          << "Bad chunk read at : " << ea << " plus : " << i;
    }
  }
}

TEST_F(Unit_ByteMapF, legacy_boundariesWithZero_1) {
  // Boundaries w/ 0's.
  const auto ea = Unit_ByteMapF::Offset + gtirb::EA{this->InitializedSize} - gtirb::EA{16};
  const auto buf = this->byteMap.getData(ea, 32);

  for (size_t i = 0; i < 32; ++i) {
    if (i >= 16) {
      EXPECT_EQ(0, buf[i]) << "Bad chunk read at : " << ea << " plus : " << i;
    } else {
      EXPECT_EQ(this->InitialByte & (i + InitializedSize - gtirb::EA{16}), buf[i])
          << "Bad chunk read at : " << ea << " plus : " << i;
    }
  }
}

TEST_F(Unit_ByteMapF, legacy_sentinelSearch_0) {
  // a. search for 0 -- should be at start of next page
  const auto ea = Unit_ByteMapF::Offset + gtirb::EA{InitializedSize} - gtirb::EA{16};
  const auto buf = this->byteMap.getDataUntil(ea, '\0', 32);

  const auto bytesRead = buf.size();

  ASSERT_EQ(17, bytesRead);

  for (size_t i = 0; i < bytesRead; ++i) {
    if (i >= 16) {
      EXPECT_EQ(0, buf[i]) << "Bad chunk read at : " << ea << " plus : " << i;
    } else {
      EXPECT_EQ(this->InitialByte & (i + InitializedSize - gtirb::EA{16}), buf[i])
          << "Bad chunk read at : " << ea << " plus : " << i;
    }
  }
}

TEST_F(Unit_ByteMapF, legacy_sentinelSearch_1) {
  // b. search for 2 -- not found, should reach limit (32)
  const auto ea = Unit_ByteMapF::Offset + gtirb::EA{InitializedSize} - gtirb::EA{32};
  const auto buf = this->byteMap.getDataUntil(ea, '\x02', 32);
  const auto bytesRead = buf.size();

  ASSERT_EQ(32, bytesRead);

  for (size_t i = 0; i < bytesRead; ++i) {
    if (i >= 32) {
      EXPECT_EQ(0, buf[i]) << "Bad chunk read at : " << ea << " plus : " << i;
    } else {
      EXPECT_EQ(this->InitialByte & (i + gtirb::EA{InitializedSize} - gtirb::EA{32}), buf[i])
          << "Bad chunk read at : " << ea << " plus : " << i;
    }
  }
}

TEST_F(Unit_ByteMapF, legacy_sentinelSearch_2) {
  // c. search for 254 -- like a, but two fewer
  const auto ea = Unit_ByteMapF::Offset + gtirb::EA{InitializedSize} - gtirb::EA{16};
  const auto buf = this->byteMap.getDataUntil(ea, static_cast<uint8_t>('\xfe'), 16);
  const auto bytesRead = buf.size();

  ASSERT_EQ(15, bytesRead);

  for (size_t i = 0; i < bytesRead; ++i) {
    if (i >= 16) {
      EXPECT_EQ(0, buf[i]) << "Bad chunk read at : " << ea << " plus : " << i;
    } else {
      EXPECT_EQ(this->InitialByte & (i + gtirb::EA{InitializedSize} - gtirb::EA{16}), buf[i])
          << "Bad chunk read at : " << ea << " plus : " << i;
    }
  }
}

TEST(Unit_ByteMap, protobufRoundTrip) {
  ByteMap original;
  uint8_t a = 'a';
  uint8_t b = 'b';
  uint8_t c = 'c';
  original.setData(EA(1), as_bytes(gsl::make_span(&a, 1)));
  original.setData(EA(2), as_bytes(gsl::make_span(&b, 1)));
  original.setData(EA(5000), as_bytes(gsl::make_span(&c, 1)));

  gtirb::ByteMap result;
  proto::ByteMap message;
  original.toProtobuf(&message);
  result.fromProtobuf(message);

  EXPECT_EQ(result.size(), original.size());
  EXPECT_EQ(result.getData(EA(1), 1)[0], 'a');
  EXPECT_EQ(result.getData(EA(2), 1)[0], 'b');
  EXPECT_EQ(result.getData(EA(5000), 1)[0], 'c');
}
