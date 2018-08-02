#include <gtirb/ImageByteMap.hpp>
#include <proto/ImageByteMap.pb.h>
#include <gtest/gtest.h>
#include <memory>

using namespace gtirb;

class Unit_ImageByteMapF : public ::testing::Test {
public:
  virtual void SetUp() override {
    EXPECT_TRUE(this->byteMap.setEAMinMax(
        {Unit_ImageByteMapF::Offset,
         Unit_ImageByteMapF::Offset + gtirb::EA{Unit_ImageByteMapF::InitializedSize}}));

    for (size_t i = 0; i < Unit_ImageByteMapF::InitializedSize; ++i) {
      const auto address = Unit_ImageByteMapF::Offset + gtirb::EA{i};

      EXPECT_NO_THROW(this->byteMap.setData(address, static_cast<uint8_t>(this->InitialByte & i)))
          << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
          << this->byteMap.getEAMinMax().second << "}.";
    }
  }

  static constexpr uint8_t InitialByte{0xFF};
  static gtirb::EA Offset;
  static size_t InitializedSize;

  gtirb::ImageByteMap byteMap{};
};

gtirb::EA Unit_ImageByteMapF::Offset{static_cast<uint64_t>(4096)};
size_t Unit_ImageByteMapF::InitializedSize{4096 * 2};

TEST(Unit_ImageByteMap, ctor_0) { EXPECT_NO_THROW(gtirb::ImageByteMap()); }

TEST(Unit_ImageByteMap, setFileName) {
  auto node = std::make_unique<gtirb::ImageByteMap>();
  ASSERT_TRUE(node != nullptr);

  const auto val = boost::filesystem::path{"/usr/local/foo"};

  EXPECT_NO_THROW(node->setFileName(val));
  EXPECT_EQ(val, node->getFileName());
}

TEST(Unit_ImageByteMap, setBaseAddress) {
  auto node = std::make_unique<gtirb::ImageByteMap>();
  ASSERT_TRUE(node != nullptr);

  const auto val = gtirb::EA{22678};

  EXPECT_NO_THROW(node->setBaseAddress(val));
  EXPECT_EQ(val, node->getBaseAddress());
}

TEST(Unit_ImageByteMap, setEntryPointAddress) {
  auto node = std::make_unique<gtirb::ImageByteMap>();
  ASSERT_TRUE(node != nullptr);

  const auto val = gtirb::EA{22678};

  EXPECT_NO_THROW(node->setEntryPointAddress(val));
  EXPECT_EQ(val, node->getEntryPointAddress());
}

TEST(Unit_ImageByteMap, setEAMinMax) {
  auto m = std::make_shared<gtirb::ImageByteMap>();

  gtirb::EA minimum{64};
  gtirb::EA maximum{1024};

  EXPECT_TRUE(m->setEAMinMax({minimum, maximum}));
  EXPECT_EQ(minimum, m->getEAMinMax().first);
  EXPECT_EQ(maximum, m->getEAMinMax().second);

  EXPECT_FALSE(m->setEAMinMax({maximum, minimum}));
  EXPECT_EQ(gtirb::EA{}, m->getEAMinMax().first);
  EXPECT_EQ(gtirb::EA{}, m->getEAMinMax().second);
}

TEST(Unit_ImageByteMap, setRebaseDelta) {
  auto node = std::make_unique<gtirb::ImageByteMap>();
  ASSERT_TRUE(node != nullptr);

  const auto val = int64_t{22678};

  EXPECT_NO_THROW(node->setRebaseDelta(val));
  EXPECT_EQ(val, node->getRebaseDelta());
}

TEST(Unit_ImageByteMap, setIsRelocated) {
  auto node = std::make_unique<gtirb::ImageByteMap>();
  ASSERT_TRUE(node != nullptr);

  const auto val = bool{true};

  EXPECT_NO_THROW(node->setIsRelocated());
  EXPECT_EQ(val, node->getIsRelocated());
}

TEST_F(Unit_ImageByteMapF, legacy_byte) {
  for (size_t i = 0; i < Unit_ImageByteMapF::InitializedSize; ++i) {
    const auto expectedWord = (((InitialByte & i) | ((InitialByte & (i + 1)) << 8)));

    EXPECT_EQ(this->InitialByte & i,
              this->byteMap.getData<uint8_t>(Unit_ImageByteMapF::Offset +
                                             gtirb::EA{static_cast<uint64_t>(i)}))
        << "Bad byte read at : " << Unit_ImageByteMapF::Offset + gtirb::EA{i};

    if (i < Unit_ImageByteMapF::InitializedSize - 1) {
      EXPECT_NO_THROW(this->byteMap.getData(Unit_ImageByteMapF::Offset + gtirb::EA{i}, 2));

      const auto word = this->byteMap.getData<uint16_t>(Unit_ImageByteMapF::Offset + gtirb::EA{i});

      EXPECT_EQ(expectedWord, word)
          << "Bad word read at : " << Unit_ImageByteMapF::Offset + gtirb::EA{i};
    }
  }
}

TEST_F(Unit_ImageByteMapF, legacy_word) {
  const auto address = gtirb::EA(0x00001000);

  ASSERT_NO_THROW(this->byteMap.setData(address, uint16_t{0xDEAD}))
      << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
      << this->byteMap.getEAMinMax().second << "}.";

  ASSERT_NO_THROW(this->byteMap.getData<uint16_t>(address))
      << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
      << this->byteMap.getEAMinMax().second << "}.";

  const auto data = this->byteMap.getData<uint16_t>(address);
  EXPECT_EQ(0xDEAD, data) << "Bad word read at : " << address;
}

TEST_F(Unit_ImageByteMapF, legacy_dword) {
  const auto address = gtirb::EA(0x00001000);

  ASSERT_NO_THROW(this->byteMap.setData(address, uint32_t{0xCAFEBABE}))
      << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
      << this->byteMap.getEAMinMax().second << "}.";

  ASSERT_NO_THROW(this->byteMap.getData<uint32_t>(address))
      << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
      << this->byteMap.getEAMinMax().second << "}.";

  const auto data = this->byteMap.getData<uint32_t>(address);
  EXPECT_EQ(0xCAFEBABE, data) << "Bad dword read at : " << address;
}

TEST_F(Unit_ImageByteMapF, legacy_qword) {
  const auto address = gtirb::EA(0x00001000);

  ASSERT_NO_THROW(this->byteMap.setData(address, uint64_t{0x8BADF00D0D15EA5E}))
      << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
      << this->byteMap.getEAMinMax().second << "}.";

  ASSERT_NO_THROW(this->byteMap.getData<uint64_t>(address))
      << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
      << this->byteMap.getEAMinMax().second << "}.";

  const auto data = this->byteMap.getData<uint64_t>(address);
  EXPECT_EQ(0x8BADF00D0D15EA5E, data) << "Bad qword read at : " << address;
}

TEST_F(Unit_ImageByteMapF, arrayData) {
  uint32_t base = std::numeric_limits<uint16_t>::max();
  std::array<uint32_t, 5> data{{base, base + 1, base + 2, base + 3, base + 4}};

  const auto address = gtirb::EA(0x00001000);

  ASSERT_NO_THROW(this->byteMap.setData(address, data))
      << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
      << this->byteMap.getEAMinMax().second << "}.";

  ASSERT_NO_THROW(this->byteMap.getData<decltype(data)>(address))
      << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
      << this->byteMap.getEAMinMax().second << "}.";

  const auto result = this->byteMap.getData<decltype(data)>(address);
  EXPECT_EQ(data, result) << "Bad array read at : " << address;
}

TEST_F(Unit_ImageByteMapF, constantData) {
  const auto address = gtirb::EA(0x00001000);

  ASSERT_NO_THROW(this->byteMap.setData(address, 32, 1))
      << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
      << this->byteMap.getEAMinMax().second << "}.";

  std::vector<uint8_t> expected(32, 1);

  ASSERT_NO_THROW(this->byteMap.getData(address, expected.size()))
      << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
      << this->byteMap.getEAMinMax().second << "}.";

  EXPECT_EQ(this->byteMap.getData(address, expected.size()), expected);
}

TEST_F(Unit_ImageByteMapF, structData) {
  struct S {
    uint32_t i;
    float f;
  };

  S data = {std::numeric_limits<uint16_t>::max() + 1, 0.1234};

  const auto address = gtirb::EA(0x00001000);

  ASSERT_NO_THROW(this->byteMap.setData(address, data))
      << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
      << this->byteMap.getEAMinMax().second << "}.";

  ASSERT_NO_THROW(this->byteMap.getData<decltype(data)>(address))
      << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
      << this->byteMap.getEAMinMax().second << "}.";

  const auto result = this->byteMap.getData<decltype(data)>(address);
  EXPECT_EQ(data.i, result.i) << "Bad struct read at : " << address;
  EXPECT_EQ(data.f, result.f) << "Bad struct read at : " << address;
}

TEST_F(Unit_ImageByteMapF, protobufRoundTrip) {
  auto& original = this->byteMap;
  const auto address = EA(0x00001000);
  ASSERT_NO_THROW(this->byteMap.setData(address, uint16_t{0xDEAD}));

  original.setFileName("test");
  original.setBaseAddress(EA(2));
  original.setEntryPointAddress(EA(3));
  original.setRebaseDelta(7);
  original.setIsRelocated();

  gtirb::ImageByteMap result;
  proto::ImageByteMap message;
  original.toProtobuf(&message);
  original.setUUID(); // Avoid UUID conflict
  result.fromProtobuf(message);

  EXPECT_EQ(result.getData<uint16_t>(address), 0xDEAD);
  EXPECT_EQ(result.getFileName(), "test");
  EXPECT_EQ(result.getBaseAddress(), EA(2));
  EXPECT_EQ(result.getEntryPointAddress(), EA(3));
  EXPECT_EQ(result.getEAMinMax(), original.getEAMinMax());
  EXPECT_EQ(result.getRebaseDelta(), 7);
  EXPECT_EQ(result.getIsRelocated(), true);
}
