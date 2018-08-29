#include <gtirb/Context.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <proto/ImageByteMap.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

class Unit_ImageByteMapF : public ::testing::Test {
public:
  virtual void SetUp() override {
    EXPECT_TRUE(this->ByteMap->setAddrMinMax(
        {Unit_ImageByteMapF::Offset,
         Unit_ImageByteMapF::Offset + Unit_ImageByteMapF::InitializedSize}));

    for (size_t i = 0; i < Unit_ImageByteMapF::InitializedSize; ++i) {
      const auto address = Unit_ImageByteMapF::Offset + i;

      EXPECT_NO_THROW(this->ByteMap->setData(
          address, static_cast<uint8_t>(this->InitialByte & i)))
          << "At Address " << static_cast<uint64_t>(address) << ", min/max={"
          << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
          << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second)
          << "}.";
    }
  }

  static constexpr uint8_t InitialByte{0xFF};
  static Addr Offset;
  static size_t InitializedSize;

  gtirb::ImageByteMap *ByteMap = ImageByteMap::Create(Ctx);
};

Addr Unit_ImageByteMapF::Offset{static_cast<uint64_t>(4096)};
size_t Unit_ImageByteMapF::InitializedSize{4096 * 2};

TEST(Unit_ImageByteMap, ctor_0) { EXPECT_NO_THROW(ImageByteMap::Create(Ctx)); }

TEST(Unit_ImageByteMap, setFileName) {
  auto *Node = ImageByteMap::Create(Ctx);
  ASSERT_TRUE(Node != nullptr);

  const std::string Val("/usr/local/foo");

  EXPECT_NO_THROW(Node->setFileName(Val));
  EXPECT_EQ(Val, Node->getFileName());
}

TEST(Unit_ImageByteMap, setBaseAddress) {
  auto *Node = ImageByteMap::Create(Ctx);
  ASSERT_TRUE(Node != nullptr);

  const auto Val = Addr{22678};

  EXPECT_NO_THROW(Node->setBaseAddress(Val));
  EXPECT_EQ(Val, Node->getBaseAddress());
}

TEST(Unit_ImageByteMap, setEntryPointAddress) {
  auto *Node = ImageByteMap::Create(Ctx);
  ASSERT_TRUE(Node != nullptr);

  const auto Val = Addr{22678};

  EXPECT_NO_THROW(Node->setEntryPointAddress(Val));
  EXPECT_EQ(Val, Node->getEntryPointAddress());
}

TEST(Unit_ImageByteMap, setEAMinMax) {
  auto *M = ImageByteMap::Create(Ctx);

  Addr Minimum{64};
  Addr Maximum{1024};

  EXPECT_TRUE(M->setAddrMinMax({Minimum, Maximum}));
  EXPECT_EQ(Minimum, M->getAddrMinMax().first);
  EXPECT_EQ(Maximum, M->getAddrMinMax().second);

  EXPECT_FALSE(M->setAddrMinMax({Maximum, Minimum}));
  EXPECT_EQ(Addr{}, M->getAddrMinMax().first);
  EXPECT_EQ(Addr{}, M->getAddrMinMax().second);
}

TEST(Unit_ImageByteMap, setRebaseDelta) {
  auto *Node = ImageByteMap::Create(Ctx);
  ASSERT_TRUE(Node != nullptr);

  const auto Val = int64_t{22678};

  EXPECT_NO_THROW(Node->setRebaseDelta(Val));
  EXPECT_EQ(Val, Node->getRebaseDelta());
}

TEST(Unit_ImageByteMap, setIsRelocated) {
  auto *Node = ImageByteMap::Create(Ctx);
  ASSERT_TRUE(Node != nullptr);

  const auto Val = bool{true};

  EXPECT_NO_THROW(Node->setIsRelocated());
  EXPECT_EQ(Val, Node->getIsRelocated());
}

TEST_F(Unit_ImageByteMapF, legacy_byte) {
  for (size_t I = 0; I < Unit_ImageByteMapF::InitializedSize; ++I) {
    const auto ExpectedWord =
        (((InitialByte & I) | ((InitialByte & (I + 1)) << 8)));

    EXPECT_EQ(this->InitialByte & I,
              this->ByteMap->getData<uint8_t>(Unit_ImageByteMapF::Offset + I))
        << "Bad byte read at : "
        << static_cast<uint64_t>(Unit_ImageByteMapF::Offset + I);

    if (I < Unit_ImageByteMapF::InitializedSize - 1) {
      EXPECT_NO_THROW(
          this->ByteMap->data(Unit_ImageByteMapF::Offset + I, 2));

      const auto Word =
          this->ByteMap->getData<uint16_t>(Unit_ImageByteMapF::Offset + I);

      EXPECT_EQ(ExpectedWord, Word)
          << "Bad word read at : "
          << static_cast<uint64_t>(Unit_ImageByteMapF::Offset + I);
    }
  }
}

TEST_F(Unit_ImageByteMapF, legacy_word) {
  const auto Address = Addr(0x00001000);

  ASSERT_NO_THROW(this->ByteMap->setData(Address, uint16_t{0xDEAD}))
      << "At Address " << static_cast<uint64_t>(Address) << ", min/max={"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second) << "}.";

  ASSERT_NO_THROW(this->ByteMap->getData<uint16_t>(Address))
      << "At Address " << static_cast<uint64_t>(Address) << ", min/max={"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second) << "}.";

  const auto Data = this->ByteMap->getData<uint16_t>(Address);
  EXPECT_EQ(0xDEAD, Data) << "Bad word read at : "
                          << static_cast<uint64_t>(Address);
}

TEST_F(Unit_ImageByteMapF, legacy_dword) {
  const auto Address = Addr(0x00001000);

  ASSERT_NO_THROW(this->ByteMap->setData(Address, uint32_t{0xCAFEBABE}))
      << "At Address " << static_cast<uint64_t>(Address) << ", min/max={"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second) << "}.";

  ASSERT_NO_THROW(this->ByteMap->getData<uint32_t>(Address))
      << "At Address " << static_cast<uint64_t>(Address) << ", min/max={"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second) << "}.";

  const auto Data = this->ByteMap->getData<uint32_t>(Address);
  EXPECT_EQ(0xCAFEBABE, Data)
      << "Bad dword read at : " << static_cast<uint64_t>(Address);
}

TEST_F(Unit_ImageByteMapF, legacy_qword) {
  const auto Address = Addr(0x00001000);

  ASSERT_NO_THROW(this->ByteMap->setData(Address, uint64_t{0x8BADF00D0D15EA5E}))
      << "At Address " << static_cast<uint64_t>(Address) << ", min/max={"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second) << "}.";

  ASSERT_NO_THROW(this->ByteMap->getData<uint64_t>(Address))
      << "At Address " << static_cast<uint64_t>(Address) << ", min/max={"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second) << "}.";

  const auto Data = this->ByteMap->getData<uint64_t>(Address);
  EXPECT_EQ(0x8BADF00D0D15EA5E, Data)
      << "Bad qword read at : " << static_cast<uint64_t>(Address);
}

TEST_F(Unit_ImageByteMapF, arrayData) {
  uint32_t base = std::numeric_limits<uint16_t>::max();
  std::array<uint32_t, 5> data{{base, base + 1, base + 2, base + 3, base + 4}};

  const auto Address = Addr(0x00001000);

  ASSERT_NO_THROW(this->ByteMap->setData(Address, data))
      << "At Address " << static_cast<uint64_t>(Address) << ", min/max={"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second) << "}.";

  ASSERT_NO_THROW(this->ByteMap->getData<decltype(data)>(Address))
      << "At Address " << static_cast<uint64_t>(Address) << ", min/max={"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second) << "}.";

  const auto Result = this->ByteMap->getData<decltype(data)>(Address);
  EXPECT_EQ(data, Result) << "Bad array read at : "
                          << static_cast<uint64_t>(Address);
}

TEST_F(Unit_ImageByteMapF, constantData) {
  const auto Address = Addr(0x00001000);

  ASSERT_NO_THROW(this->ByteMap->setData(Address, 32, gsl::byte(1)))
      << "At Address " << static_cast<uint64_t>(Address) << ", min/max={"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second) << "}.";

  std::vector<std::byte> Expected(32, std::byte(1));

  ASSERT_NO_THROW(this->ByteMap->data(Address, Expected.size()))
      << "At Address " << static_cast<uint64_t>(Address) << ", min/max={"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second) << "}.";

  EXPECT_EQ(this->ByteMap->data(Address, Expected.size()), Expected);
}

struct TestStruct {
  uint32_t I;
  uint8_t J;
};

TestStruct endian_reverse(const TestStruct& X) {
  return {boost::endian::endian_reverse(X.I), X.J};
}

TEST_F(Unit_ImageByteMapF, structData) {
  TestStruct Data = {
      static_cast<uint32_t>(std::numeric_limits<uint16_t>::max() + 1), 0xAB};

  const auto Address = Addr(0x00001000);

  ASSERT_NO_THROW(this->ByteMap->setData(Address, Data))
      << "At Address " << static_cast<uint64_t>(Address) << ", min/max={"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second) << "}.";

  ASSERT_NO_THROW(this->ByteMap->getData<decltype(Data)>(Address))
      << "At Address " << static_cast<uint64_t>(Address) << ", min/max={"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second) << "}.";

  const auto Result = this->ByteMap->getData<decltype(Data)>(Address);
  EXPECT_EQ(Data.I, Result.I)
      << "Bad struct read at : " << static_cast<uint64_t>(Address);
  EXPECT_EQ(Data.J, Result.J)
      << "Bad struct read at : " << static_cast<uint64_t>(Address);
}

TEST_F(Unit_ImageByteMapF, littleEndian) {
  this->ByteMap->setByteOrder(boost::endian::order::little);

  const uint16_t W = 0xABCD;
  const uint32_t Dw = 0xDEADBEEF;
  const uint64_t Qw = 0xFEEDFACEABCDEF01;
  std::array<uint16_t, 2> A = {{0xACAF, 0xBFAE}};
  const TestStruct S = {0xBAADF00D, 0xAB};

  Addr Addr0(0x1000);
  Addr Addr1 = Addr0 + sizeof(W);
  Addr Addr2 = Addr1 + sizeof(Dw);
  Addr Addr3 = Addr2 + sizeof(Qw);
  Addr Addr4 = Addr3 + sizeof(A);
  this->ByteMap->setData(Addr0, W);
  this->ByteMap->setData(Addr1, Dw);
  this->ByteMap->setData(Addr2, Qw);
  this->ByteMap->setData(Addr3, A);
  this->ByteMap->setData(Addr4, S);

  // Confirm expected byte order
  std::vector<std::byte> expected{
      // w
      std::byte(0xCD), std::byte(0xAB),
      // dw
      std::byte(0xEF), std::byte(0xBE), std::byte(0xAD), std::byte(0xDE),
      // qw
      std::byte(0x01), std::byte(0xEF), std::byte(0xCD), std::byte(0xAB),
      std::byte(0xCE), std::byte(0xFA), std::byte(0xED), std::byte(0xFE),
      // a
      std::byte(0xAF), std::byte(0xAC), std::byte(0xAE), std::byte(0xBF),
      // s
      std::byte(0x0D), std::byte(0xF0), std::byte(0xAD), std::byte(0xBA),
      std::byte(0xAB)};

  // Skip padding of s
  size_t Size = sizeof(W) + sizeof(Dw) + sizeof(Qw) + sizeof(A) + sizeof(S.I) +
                sizeof(S.J);
  EXPECT_EQ(this->ByteMap->data(Addr0, Size), expected);

  // Confirm that getData returns to native order.
  EXPECT_EQ(this->ByteMap->getData<uint16_t>(Addr0), W);
  EXPECT_EQ(this->ByteMap->getData<uint32_t>(Addr1), Dw);
  EXPECT_EQ(this->ByteMap->getData<uint64_t>(Addr2), Qw);
  EXPECT_EQ(this->ByteMap->getData<decltype(A)>(Addr3), A);
  TestStruct S2 = this->ByteMap->getData<TestStruct>(Addr4);
  EXPECT_EQ(S2.I, S.I);
  EXPECT_EQ(S2.J, S.J);
}

TEST_F(Unit_ImageByteMapF, bigEndian) {
  this->ByteMap->setByteOrder(boost::endian::order::big);

  const uint16_t W = 0xABCD;
  const uint32_t Dw = 0xDEADBEEF;
  const uint64_t Qw = 0xFEEDFACEABCDEF01;
  std::array<uint16_t, 2> A = {{0xACAF, 0xBFAE}};
  const TestStruct S = {0xBAADF00D, 0xAB};

  Addr Addr0(0x1000);
  Addr Addr1 = Addr0 + sizeof(W);
  Addr Addr2 = Addr1 + sizeof(Dw);
  Addr Addr3 = Addr2 + sizeof(Qw);
  Addr Addr4 = Addr3 + sizeof(A);
  this->ByteMap->setData(Addr0, W);
  this->ByteMap->setData(Addr1, Dw);
  this->ByteMap->setData(Addr2, Qw);
  this->ByteMap->setData(Addr3, A);
  this->ByteMap->setData(Addr4, S);

  // Confirm expected byte order
  std::vector<std::byte> Expected{
      // w
      std::byte(0xAB),
      std::byte(0xCD),
      // dw
      std::byte(0xDE),
      std::byte(0xAD),
      std::byte(0xBE),
      std::byte(0xEF),
      // qw
      std::byte(0xFE),
      std::byte(0xED),
      std::byte(0xFA),
      std::byte(0xCE),
      std::byte(0xAB),
      std::byte(0xCD),
      std::byte(0xEF),
      std::byte(0x01),
      // a
      std::byte(0xAC),
      std::byte(0xAF),
      std::byte(0xBF),
      std::byte(0xAE),
      // s
      std::byte(0xBA),
      std::byte(0xAD),
      std::byte(0xF0),
      std::byte(0x0D),
      std::byte(0xAB),
  };

  // Skip padding of s
  size_t Size = sizeof(W) + sizeof(Dw) + sizeof(Qw) + sizeof(A) + sizeof(S.I) +
                sizeof(S.J);
  EXPECT_EQ(this->ByteMap->data(Addr0, Size), Expected);

  // Confirm that getData returns to native order.
  EXPECT_EQ(this->ByteMap->getData<uint16_t>(Addr0), W);
  EXPECT_EQ(this->ByteMap->getData<uint32_t>(Addr1), Dw);
  EXPECT_EQ(this->ByteMap->getData<uint64_t>(Addr2), Qw);
  EXPECT_EQ(this->ByteMap->getData<decltype(A)>(Addr3), A);
  TestStruct S2 = this->ByteMap->getData<TestStruct>(Addr4);
  EXPECT_EQ(S2.I, S.I);
  EXPECT_EQ(S2.J, S.J);
}

TEST_F(Unit_ImageByteMapF, protobufRoundTrip) {
  auto *Original = this->ByteMap;
  const auto Address = Addr(0x00001000);
  ASSERT_NO_THROW(this->ByteMap->setData(Address, uint16_t{0xDEAD}));

  Original->setFileName("test");
  Original->setBaseAddress(Addr(2));
  Original->setEntryPointAddress(Addr(3));
  Original->setRebaseDelta(7);
  Original->setIsRelocated();

  proto::ImageByteMap Message;
  Original->toProtobuf(&Message);
  details::ClearUUIDs(Original); // Avoid UUID conflict
  ImageByteMap *Result = ImageByteMap::fromProtobuf(Ctx, Message);

  EXPECT_EQ(Result->getData<uint16_t>(Address), 0xDEAD);
  EXPECT_EQ(Result->getFileName(), "test");
  EXPECT_EQ(Result->getBaseAddress(), Addr(2));
  EXPECT_EQ(Result->getEntryPointAddress(), Addr(3));
  EXPECT_EQ(Result->getAddrMinMax(), Original->getAddrMinMax());
  EXPECT_EQ(Result->getRebaseDelta(), 7);
  EXPECT_EQ(Result->getIsRelocated(), true);
}
