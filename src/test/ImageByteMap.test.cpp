//===- ImageByteMap.test.cpp ------------------------------------*- C++ -*-===//
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
#include <gtirb/ImageByteMap.hpp>
#include <proto/ImageByteMap.pb.h>
#include <cstring>
#include <gtest/gtest.h>
#include <iterator>
#include <list>
#include <type_traits>

using namespace gtirb;

template <typename RangeTy> static bool empty(RangeTy&& R) {
  return std::distance(std::forward<RangeTy>(R).begin(),
                       std::forward<RangeTy>(R).end()) == 0;
}

static Context Ctx;

class Unit_ImageByteMapF : public ::testing::Test {
public:
  virtual void SetUp() override {
    EXPECT_TRUE(this->ByteMap->setAddrMinMax(
        {Unit_ImageByteMapF::Offset,
         Unit_ImageByteMapF::Offset + Unit_ImageByteMapF::InitializedSize}));

    for (size_t i = 0; i < Unit_ImageByteMapF::InitializedSize; ++i) {
      const auto address = Unit_ImageByteMapF::Offset + i;

      EXPECT_TRUE(this->ByteMap->setData(
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

  gtirb::ImageByteMap* ByteMap = ImageByteMap::Create(Ctx);
};

Addr Unit_ImageByteMapF::Offset{static_cast<uint64_t>(4096)};
size_t Unit_ImageByteMapF::InitializedSize{4096 * 2};

TEST(Unit_ImageByteMap, ctor_0) {
  EXPECT_NE(ImageByteMap::Create(Ctx), nullptr);
}

TEST(Unit_ImageByteMap, setBaseAddress) {
  auto* Node = ImageByteMap::Create(Ctx);
  ASSERT_TRUE(Node != nullptr);

  const auto Val = Addr{22678};

  Node->setBaseAddress(Val);
  EXPECT_EQ(Val, Node->getBaseAddress());
}

TEST(Unit_ImageByteMap, setEntryPointAddress) {
  auto* Node = ImageByteMap::Create(Ctx);
  ASSERT_TRUE(Node != nullptr);

  const auto Val = Addr{22678};

  Node->setEntryPointAddress(Val);
  EXPECT_EQ(Val, Node->getEntryPointAddress());
}

TEST(Unit_ImageByteMap, setAddrMinMax) {
  auto* M = ImageByteMap::Create(Ctx);

  Addr Minimum{64};
  Addr Maximum{1024};

  EXPECT_TRUE(M->setAddrMinMax({Minimum, Maximum}));
  EXPECT_EQ(Minimum, M->getAddrMinMax().first);
  EXPECT_EQ(Maximum, M->getAddrMinMax().second);

  // Invalid range: return false and leave value unchanged.
  EXPECT_FALSE(M->setAddrMinMax({Maximum, Minimum}));
  EXPECT_EQ(Minimum, M->getAddrMinMax().first);
  EXPECT_EQ(Maximum, M->getAddrMinMax().second);
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
      EXPECT_FALSE(
          empty(this->ByteMap->data(Unit_ImageByteMapF::Offset + I, 2)));

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

  EXPECT_TRUE(this->ByteMap->setData(Address, uint16_t{0xDEAD}))
      << "At Address " << static_cast<uint64_t>(Address) << ", min/max={"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second) << "}.";

  const auto Data = this->ByteMap->getData<uint16_t>(Address);
  EXPECT_EQ(0xDEAD, Data) << "Bad word read at : "
                          << static_cast<uint64_t>(Address);
}

TEST_F(Unit_ImageByteMapF, legacy_dword) {
  const auto Address = Addr(0x00001000);

  EXPECT_TRUE(this->ByteMap->setData(Address, uint32_t{0xCAFEBABE}))
      << "At Address " << static_cast<uint64_t>(Address) << ", min/max={"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second) << "}.";

  const auto Data = this->ByteMap->getData<uint32_t>(Address);
  EXPECT_EQ(0xCAFEBABE, Data)
      << "Bad dword read at : " << static_cast<uint64_t>(Address);
}

TEST_F(Unit_ImageByteMapF, legacy_qword) {
  const auto Address = Addr(0x00001000);

  EXPECT_TRUE(this->ByteMap->setData(Address, uint64_t{0x8BADF00D0D15EA5E}))
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

  EXPECT_TRUE(this->ByteMap->setData(Address, data))
      << "At Address " << static_cast<uint64_t>(Address) << ", min/max={"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second) << "}.";

  const auto Result = this->ByteMap->getData<decltype(data)>(Address);
  EXPECT_EQ(data, Result) << "Bad array read at : "
                          << static_cast<uint64_t>(Address);
}

TEST_F(Unit_ImageByteMapF, constantData) {
  const auto Address = Addr(0x00001000);

  EXPECT_TRUE(this->ByteMap->setData(Address, 32, std::byte(1)))
      << "At Address " << static_cast<uint64_t>(Address) << ", min/max={"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second) << "}.";

  std::vector<std::byte> Expected(32, std::byte(1));
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

  EXPECT_TRUE(this->ByteMap->setData(Address, Data))
      << "At Address " << static_cast<uint64_t>(Address) << ", min/max={"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().first) << "/"
      << static_cast<uint64_t>(this->ByteMap->getAddrMinMax().second) << "}.";

  const auto Result = this->ByteMap->getData<decltype(Data)>(Address);
  EXPECT_TRUE(Result.has_value());
  EXPECT_EQ(Data.I, Result->I)
      << "Bad struct read at : " << static_cast<uint64_t>(Address);
  EXPECT_EQ(Data.J, Result->J)
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
  EXPECT_TRUE(this->ByteMap->setData(Addr0, W));
  EXPECT_TRUE(this->ByteMap->setData(Addr1, Dw));
  EXPECT_TRUE(this->ByteMap->setData(Addr2, Qw));
  EXPECT_TRUE(this->ByteMap->setData(Addr3, A));
  EXPECT_TRUE(this->ByteMap->setData(Addr4, S));

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
  std::optional<TestStruct> S2 = this->ByteMap->getData<TestStruct>(Addr4);
  EXPECT_TRUE(S2.has_value());
  EXPECT_EQ(S2->I, S.I);
  EXPECT_EQ(S2->J, S.J);
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
  EXPECT_TRUE(this->ByteMap->setData(Addr0, W));
  EXPECT_TRUE(this->ByteMap->setData(Addr1, Dw));
  EXPECT_TRUE(this->ByteMap->setData(Addr2, Qw));
  EXPECT_TRUE(this->ByteMap->setData(Addr3, A));
  EXPECT_TRUE(this->ByteMap->setData(Addr4, S));

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
  std::optional<TestStruct> S2 = this->ByteMap->getData<TestStruct>(Addr4);
  EXPECT_TRUE(S2.has_value());
  EXPECT_EQ(S2->I, S.I);
  EXPECT_EQ(S2->J, S.J);
}

TEST_F(Unit_ImageByteMapF, protobufRoundTrip) {
  auto* Original = this->ByteMap;
  const auto Address = Addr(0x00001000);
  EXPECT_TRUE(this->ByteMap->setData(Address, uint16_t{0xDEAD}));

  Original->setBaseAddress(Addr(2));
  Original->setEntryPointAddress(Addr(3));

  proto::ImageByteMap Message;
  Original->toProtobuf(&Message);

  Context OuterCtx;
  ImageByteMap* Result = ImageByteMap::fromProtobuf(OuterCtx, Message);

  EXPECT_EQ(Result->getData<uint16_t>(Address), 0xDEAD);
  EXPECT_EQ(Result->getBaseAddress(), Addr(2));
  EXPECT_EQ(Result->getEntryPointAddress(), Addr(3));
  EXPECT_EQ(Result->getAddrMinMax(), Original->getAddrMinMax());
}

TEST(Unit_ImageByteMap, contiguousIterators) {
  ImageByteMap* IBM = ImageByteMap::Create(Ctx);

  std::vector<std::byte> OriginalBytes = {
      std::byte(0), std::byte(1), std::byte(2), std::byte(3), std::byte(4),
      std::byte(5), std::byte(6), std::byte(7), std::byte(8), std::byte(9)};

  IBM->setAddrMinMax(std::make_pair(Addr(0), Addr(10)));

  EXPECT_TRUE(IBM->setData(Addr(0), boost::make_iterator_range(OriginalBytes)));

  // Ensure that the iterators returned by ImageByteMap are contiguous.
  std::vector<std::byte> CopiedBytes(OriginalBytes.size());
  auto R = IBM->data(Addr(0), OriginalBytes.size());
  std::memcpy(&CopiedBytes[0], &*R.begin(), R.end() - R.begin());

  EXPECT_TRUE(std::equal(OriginalBytes.begin(), OriginalBytes.end(),
                         CopiedBytes.begin(), CopiedBytes.end()));
}

TEST(Unit_ImageByteMap, overlappingRegions) {
  // Ensure that you can not use the ImageByteMap to create an overlapping
  // byte map region, and that failed attempts to do so do not mutate existing
  // data.
  ImageByteMap* IBM = ImageByteMap::Create(Ctx);

  std::vector<std::byte> OriginalBytes = {
      std::byte(0), std::byte(1), std::byte(2), std::byte(3), std::byte(4),
      std::byte(5), std::byte(6), std::byte(7), std::byte(8), std::byte(9)};

  IBM->setAddrMinMax(std::make_pair(Addr(0), Addr(25)));
  EXPECT_TRUE(IBM->setData(Addr(0), boost::make_iterator_range(OriginalBytes)));
  EXPECT_TRUE(
      IBM->setData(Addr(12), boost::make_iterator_range(OriginalBytes)));

  // There should be data in regions [0..9] and [12..21], which means that
  // setting six bytes between [8..13] should cause overlap.
  std::array<std::byte, 6> NewBytes = {std::byte(100), std::byte(101),
                                       std::byte(102), std::byte(103),
                                       std::byte(104), std::byte(105)};
  EXPECT_FALSE(IBM->setData(Addr(8), NewBytes));

  auto Data = IBM->getData<10>(Addr(0));
  EXPECT_TRUE(Data.has_value());
  EXPECT_TRUE(std::equal(OriginalBytes.begin(), OriginalBytes.end(),
                         Data->begin(), Data->end()));

  // Similar for memsetting bytes.
  EXPECT_FALSE(IBM->setData(Addr(8), 6, std::byte{100}));

  Data = IBM->getData<10>(Addr(0));
  EXPECT_TRUE(Data.has_value());
  EXPECT_TRUE(std::equal(OriginalBytes.begin(), OriginalBytes.end(),
                         Data->begin(), Data->end()));

  // Setting [10..12] should also fail. [10..11] are available, but [12] is
  // owned by the second region.
  std::array<std::byte, 3> OtherNewBytes = {std::byte(100), std::byte(101),
                                            std::byte(102)};
  EXPECT_FALSE(IBM->setData(Addr(10), OtherNewBytes));

  Data = IBM->getData<10>(Addr(0));
  EXPECT_TRUE(Data.has_value());
  EXPECT_TRUE(std::equal(OriginalBytes.begin(), OriginalBytes.end(),
                         Data->begin(), Data->end()));

  // Similar for memsetting bytes.
  EXPECT_FALSE(IBM->setData(Addr(10), 3, std::byte{100}));

  Data = IBM->getData<10>(Addr(0));
  EXPECT_TRUE(Data.has_value());
  EXPECT_TRUE(std::equal(OriginalBytes.begin(), OriginalBytes.end(),
                         Data->begin(), Data->end()));

  // However, we can extend the first region with two more bytes.
  EXPECT_TRUE(IBM->setData(Addr(10), 2, std::byte{100}));

  auto ExtendedData = IBM->getData<12>(Addr(0));
  EXPECT_TRUE(ExtendedData.has_value());
  EXPECT_TRUE(std::equal(OriginalBytes.begin(), OriginalBytes.end(),
                         ExtendedData->begin(), ExtendedData->end() - 2));
  EXPECT_EQ((*ExtendedData)[10], std::byte(100));
  EXPECT_EQ((*ExtendedData)[11], std::byte(100));
}

// Test different types of sources of data for initializing a byte map.
TEST(Unit_ImageByteMap, arbitraryData) {
  ImageByteMap* IBM = ImageByteMap::Create(Ctx);

  IBM->setAddrMinMax(std::make_pair(Addr(0), Addr(100)));

  // std::array of int
  std::array<uint32_t, 2> data = {1, 2};
  EXPECT_TRUE(IBM->setData(Addr(0), data));
  EXPECT_TRUE(IBM->getData<uint32_t>(Addr(0)) == data[0]);
  EXPECT_TRUE(IBM->getData<uint32_t>(Addr(4)) == data[1]);

  // Iterator range over std::array
  EXPECT_TRUE(IBM->setData(Addr(8), boost::make_iterator_range(data)));
  EXPECT_TRUE(IBM->getData<uint32_t>(Addr(8)) == data[0]);
  EXPECT_TRUE(IBM->getData<uint32_t>(Addr(12)) == data[1]);

  // Some data type that doesn't store things in an array.
  std::list<uint64_t> data_as_list = {1, 2, 3};
  Addr a = Addr(16);
  EXPECT_TRUE(IBM->setData(a, boost::make_iterator_range(data_as_list)));
  for (auto item : data_as_list) {
    EXPECT_TRUE(IBM->getData<uint64_t>(a) == item);
    a += sizeof(uint64_t);
  }

  // Overlapping region
  a = Addr(17);
  EXPECT_FALSE(IBM->setData(a, boost::make_iterator_range(data_as_list)));
  // Expect original data to be unchanged
  a = Addr(16);
  for (auto item : data_as_list) {
    EXPECT_TRUE(IBM->getData<uint64_t>(a) == item);
    a += sizeof(uint64_t);
  }
}
