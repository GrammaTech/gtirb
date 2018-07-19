#include <gtest/gtest.h>
#include <proto/ImageByteMap.pb.h>
#include <gtirb/ImageByteMap.hpp>
#include <memory>

using namespace gtirb;

class Unit_ImageByteMapF : public ::testing::Test
{
public:
    virtual void SetUp() override
    {
        EXPECT_TRUE(this->byteMap.setEAMinMax(
            {Unit_ImageByteMapF::Offset,
             Unit_ImageByteMapF::Offset + gtirb::EA{Unit_ImageByteMapF::InitializedSize}}));

        for(size_t i = 0; i < Unit_ImageByteMapF::InitializedSize; ++i)
        {
            const auto address = Unit_ImageByteMapF::Offset + gtirb::EA{i};

            EXPECT_NO_THROW(
                this->byteMap.setData(address, static_cast<uint8_t>(this->InitialByte & i)))
                << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first
                << "/" << this->byteMap.getEAMinMax().second << "}.";
        }
    }

    static constexpr uint8_t InitialByte{0xFF};
    static gtirb::EA Offset;
    static size_t InitializedSize;

    gtirb::ImageByteMap byteMap{};
};

gtirb::EA Unit_ImageByteMapF::Offset{static_cast<uint64_t>(gtirb::constants::PageSize)};
size_t Unit_ImageByteMapF::InitializedSize{gtirb::constants::PageSize * 2};

TEST(Unit_ImageByteMap, ctor_0)
{
    EXPECT_NO_THROW(gtirb::ImageByteMap());
}

TEST(Unit_ImageByteMap, setFileName)
{
    auto node = std::make_unique<gtirb::ImageByteMap>();
    ASSERT_TRUE(node != nullptr);

    const auto val = boost::filesystem::path{"/usr/local/foo"};

    EXPECT_NO_THROW(node->setFileName(val));
    EXPECT_EQ(val, node->getFileName());
}

TEST(Unit_ImageByteMap, setBaseAddress)
{
    auto node = std::make_unique<gtirb::ImageByteMap>();
    ASSERT_TRUE(node != nullptr);

    const auto val = gtirb::EA{22678};

    EXPECT_NO_THROW(node->setBaseAddress(val));
    EXPECT_EQ(val, node->getBaseAddress());
}

TEST(Unit_ImageByteMap, setEntryPointAddress)
{
    auto node = std::make_unique<gtirb::ImageByteMap>();
    ASSERT_TRUE(node != nullptr);

    const auto val = gtirb::EA{22678};

    EXPECT_NO_THROW(node->setEntryPointAddress(val));
    EXPECT_EQ(val, node->getEntryPointAddress());
}

TEST(Unit_ImageByteMap, setEAMinMax)
{
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

TEST(Unit_ImageByteMap, setRebaseDelta)
{
    auto node = std::make_unique<gtirb::ImageByteMap>();
    ASSERT_TRUE(node != nullptr);

    const auto val = int64_t{22678};

    EXPECT_NO_THROW(node->setRebaseDelta(val));
    EXPECT_EQ(val, node->getRebaseDelta());
}

TEST(Unit_ImageByteMap, setLFCM)
{
    auto node = std::make_unique<gtirb::ImageByteMap>();
    ASSERT_TRUE(node != nullptr);

    const auto val = uint8_t{gtirb::ImageByteMap::CM_N8_F16 | gtirb::ImageByteMap::CC_CDECL
                             | gtirb::ImageByteMap::MM_NN};

    EXPECT_NO_THROW(node->setLFCM(val));
    EXPECT_EQ(val, node->getLFCM());
}

TEST(Unit_ImageByteMap, setIsRelocated)
{
    auto node = std::make_unique<gtirb::ImageByteMap>();
    ASSERT_TRUE(node != nullptr);

    const auto val = bool{true};

    EXPECT_NO_THROW(node->setIsRelocated());
    EXPECT_EQ(val, node->getIsRelocated());
}

TEST(Unit_ImageByteMap, setGlobalOffsetTableAddress)
{
    auto node = std::make_unique<gtirb::ImageByteMap>();
    ASSERT_TRUE(node != nullptr);

    const auto val = gtirb::EA{22678};

    EXPECT_NO_THROW(node->setGlobalOffsetTableAddress(val));
    EXPECT_EQ(val, node->getGlobalOffsetTableAddress());
}

TEST(Unit_ImageByteMap, setContentSource)
{
    auto node = std::make_unique<gtirb::ImageByteMap>();
    ASSERT_TRUE(node != nullptr);

    const auto val = gtirb::ImageByteMap::ContentSource::IDAPartial;

    EXPECT_NO_THROW(node->setContentSource(val));
    EXPECT_EQ(val, node->getContentSource());
}

TEST_F(Unit_ImageByteMapF, empty)
{
    EXPECT_FALSE(this->byteMap.getDataEmpty());
}

TEST_F(Unit_ImageByteMapF, size)
{
    EXPECT_EQ(Unit_ImageByteMapF::InitializedSize, this->byteMap.getDataSize());
}

TEST_F(Unit_ImageByteMapF, legacy_byte)
{
    for(size_t i = 0; i < Unit_ImageByteMapF::InitializedSize; ++i)
    {
        const auto expectedWord = (((InitialByte & i) | ((InitialByte & (i + 1)) << 8)));

        EXPECT_EQ(this->InitialByte & i,
                  this->byteMap.getData8(Unit_ImageByteMapF::Offset
                                         + gtirb::EA{static_cast<uint64_t>(i)}))
            << "Bad byte read at : " << Unit_ImageByteMapF::Offset + gtirb::EA{i};

        if(i < Unit_ImageByteMapF::InitializedSize - 1)
        {
            EXPECT_NO_THROW(this->byteMap.getData(Unit_ImageByteMapF::Offset + gtirb::EA{i}, 2));

            const auto word = this->byteMap.getData16(Unit_ImageByteMapF::Offset + gtirb::EA{i});

            EXPECT_EQ(expectedWord, word)
                << "Bad word read at : " << Unit_ImageByteMapF::Offset + gtirb::EA{i};
        }
    }
}

TEST_F(Unit_ImageByteMapF, legacy_word)
{
    const auto address = gtirb::EA(0x00001000);

    ASSERT_NO_THROW(this->byteMap.setData(address, uint16_t{0xDEAD}))
        << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
        << this->byteMap.getEAMinMax().second << "}.";

    ASSERT_NO_THROW(this->byteMap.getData16(address))
        << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
        << this->byteMap.getEAMinMax().second << "}.";

    const auto data = this->byteMap.getData16(address);
    EXPECT_EQ(0xDEAD, data) << "Bad word read at : " << address;
}

TEST_F(Unit_ImageByteMapF, legacy_dword)
{
    const auto address = gtirb::EA(0x00001000);

    ASSERT_NO_THROW(this->byteMap.setData(address, uint32_t{0xCAFEBABE}))
        << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
        << this->byteMap.getEAMinMax().second << "}.";

    ASSERT_NO_THROW(this->byteMap.getData32(address))
        << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
        << this->byteMap.getEAMinMax().second << "}.";

    const auto data = this->byteMap.getData32(address);
    EXPECT_EQ(0xCAFEBABE, data) << "Bad dword read at : " << address;
}

TEST_F(Unit_ImageByteMapF, legacy_qword)
{
    const auto address = gtirb::EA(0x00001000);

    ASSERT_NO_THROW(this->byteMap.setData(address, uint64_t{0x8BADF00D0D15EA5E}))
        << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
        << this->byteMap.getEAMinMax().second << "}.";

    ASSERT_NO_THROW(this->byteMap.getData64(address))
        << "At Address " << address << ", min/max={" << this->byteMap.getEAMinMax().first << "/"
        << this->byteMap.getEAMinMax().second << "}.";

    const auto data = this->byteMap.getData64(address);
    EXPECT_EQ(0x8BADF00D0D15EA5E, data) << "Bad qword read at : " << address;
}

TEST_F(Unit_ImageByteMapF, legacy_sentinelSearch_0)
{
    // a. search for 0 -- should be at start of next page
    const auto ea = Unit_ImageByteMapF::Offset + gtirb::EA{InitializedSize} - gtirb::EA{16};
    const auto buf = this->byteMap.getDataUntil(ea, '\0', 32);

    const auto bytesRead = buf.size();

    ASSERT_EQ(17, bytesRead);

    for(size_t i = 0; i < bytesRead; ++i)
    {
        if(i >= 16)
        {
            EXPECT_EQ(0, buf[i]) << "Bad chunk read at : " << ea << " plus : " << i;
        }
        else
        {
            EXPECT_EQ(this->InitialByte & (i + InitializedSize - gtirb::EA{16}), buf[i])
                << "Bad chunk read at : " << ea << " plus : " << i;
        }
    }
}

TEST_F(Unit_ImageByteMapF, legacy_sentinelSearch_1)
{
    // b. search for 2 -- not found, should reach limit (32)
    const auto ea = Unit_ImageByteMapF::Offset + gtirb::EA{InitializedSize} - gtirb::EA{32};
    const auto buf = this->byteMap.getDataUntil(ea, '\x02', 32);
    const auto bytesRead = buf.size();

    ASSERT_EQ(32, bytesRead);

    for(size_t i = 0; i < bytesRead; ++i)
    {
        if(i >= 32)
        {
            EXPECT_EQ(0, buf[i]) << "Bad chunk read at : " << ea << " plus : " << i;
        }
        else
        {
            EXPECT_EQ(this->InitialByte & (i + gtirb::EA{InitializedSize} - gtirb::EA{32}), buf[i])
                << "Bad chunk read at : " << ea << " plus : " << i;
        }
    }
}

TEST_F(Unit_ImageByteMapF, legacy_sentinelSearch_2)
{
    // c. search for 254 -- like a, but two fewer
    const auto ea = Unit_ImageByteMapF::Offset + gtirb::EA{InitializedSize} - gtirb::EA{16};
    const auto buf = this->byteMap.getDataUntil(ea, static_cast<uint8_t>('\xfe'), 16);
    const auto bytesRead = buf.size();

    ASSERT_EQ(15, bytesRead);

    for(size_t i = 0; i < bytesRead; ++i)
    {
        if(i >= 16)
        {
            EXPECT_EQ(0, buf[i]) << "Bad chunk read at : " << ea << " plus : " << i;
        }
        else
        {
            EXPECT_EQ(this->InitialByte & (i + gtirb::EA{InitializedSize} - gtirb::EA{16}), buf[i])
                << "Bad chunk read at : " << ea << " plus : " << i;
        }
    }
}

TEST_F(Unit_ImageByteMapF, protobufRoundTrip)
{
    auto &original = this->byteMap;
    const auto address = EA(0x00001000);
    ASSERT_NO_THROW(this->byteMap.setData(address, uint16_t{0xDEAD}));

    original.setFileName("test");
    original.setBaseAddress(EA(2));
    original.setEntryPointAddress(EA(3));
    original.setRebaseDelta(7);
    original.setLFCM(ImageByteMap::MM_MASK);
    original.setIsRelocated();
    original.setGlobalOffsetTableAddress(EA(8));
    original.setContentSource(ImageByteMap::ContentSource::IDAFull);

    gtirb::ImageByteMap result;
    proto::ImageByteMap message;
    original.toProtobuf(&message);
    original.setUUID(); // Avoid UUID conflict
    result.fromProtobuf(message);

    EXPECT_EQ(result.getDataSize(), original.getDataSize());
    EXPECT_EQ(result.getData16(address), 0xDEAD);
    EXPECT_EQ(result.getFileName(), "test");
    EXPECT_EQ(result.getBaseAddress(), EA(2));
    EXPECT_EQ(result.getEntryPointAddress(), EA(3));
    EXPECT_EQ(result.getEAMinMax(), original.getEAMinMax());
    EXPECT_EQ(result.getRebaseDelta(), 7);
    EXPECT_EQ(result.getLFCM(), ImageByteMap::MM_MASK);
    EXPECT_EQ(result.getIsRelocated(), true);
    EXPECT_EQ(result.getGlobalOffsetTableAddress(), EA(8));
    EXPECT_EQ(result.getContentSource(), ImageByteMap::ContentSource::IDAFull);
}
