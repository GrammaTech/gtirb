#include <gtest/gtest.h>
#include <boost/archive/polymorphic_text_iarchive.hpp>
#include <boost/archive/polymorphic_text_oarchive.hpp>
#include <boost/filesystem.hpp>
#include <gtirb/ByteMap.hpp>
#include <gtirb/Constants.hpp>
#include <gtirb/Utilities.hpp>
#include <memory>

class Unit_ByteMapF : public ::testing::Test
{
public:
    virtual void SetUp() override
    {
        for(size_t i = 0; i < this->InitializedSize; ++i)
        {
            this->byteMap.setData(Unit_ByteMapF::Offset + gtirb::EA{i},
                                  static_cast<uint8_t>(this->InitialByte & i));
        }
    }

    static constexpr uint8_t InitialByte{0xFF};
    static gtirb::EA Offset;
    static size_t InitializedSize;

    gtirb::ByteMap byteMap{};
};

gtirb::EA Unit_ByteMapF::Offset{static_cast<uint64_t>(gtirb::constants::PageSize)};
size_t Unit_ByteMapF::InitializedSize{gtirb::constants::PageSize * 2};

TEST(Unit_ByteMap, ctor_0)
{
    EXPECT_NO_THROW(gtirb::ByteMap());
}

TEST(Unit_ByteMap, empty_0)
{
    auto byteMap = gtirb::ByteMap();
    EXPECT_TRUE(byteMap.empty());
}

TEST(Unit_ByteMap, size_0)
{
    auto byteMap = gtirb::ByteMap();
    EXPECT_EQ(size_t{0}, byteMap.size());
}

TEST_F(Unit_ByteMapF, empty)
{
    EXPECT_FALSE(this->byteMap.empty());
}

TEST_F(Unit_ByteMapF, size)
{
    EXPECT_EQ(this->InitializedSize, this->byteMap.size());
}

TEST_F(Unit_ByteMapF, legacy_byte)
{
    for(size_t i = 0; i < this->InitializedSize; ++i)
    {
        const auto expectedWord = (((InitialByte & i) | ((InitialByte & (i + 1)) << 8)));

        EXPECT_EQ(
            this->InitialByte & i,
            this->byteMap.getData8(Unit_ByteMapF::Offset + gtirb::EA{static_cast<uint64_t>(i)}))
            << "Bad byte read at : " << Unit_ByteMapF::Offset + gtirb::EA{i};

        if(i < this->InitializedSize - 1)
        {
            EXPECT_NO_THROW(this->byteMap.getData(Unit_ByteMapF::Offset + gtirb::EA{i}, 2));

            const auto word = this->byteMap.getData16(Unit_ByteMapF::Offset + gtirb::EA{i});

            EXPECT_EQ(expectedWord, word) << "Bad word read at : "
                                          << Unit_ByteMapF::Offset + gtirb::EA{i};
        }
    }
}

TEST_F(Unit_ByteMapF, legacy_word)
{
    this->byteMap.setData(gtirb::EA(0x401000), uint16_t{0xDEAD});
    EXPECT_EQ(0xDEAD, this->byteMap.getData16(gtirb::EA(0x401000))) << "Bad word read at : "
                                                                    << 0x401000;
}

TEST_F(Unit_ByteMapF, legacy_dword)
{
    this->byteMap.setData(gtirb::EA(0x402000), uint32_t{0xCAFEBABE});
    EXPECT_EQ(uint32_t{0xCAFEBABE}, this->byteMap.getData32(gtirb::EA(0x402000)))
        << "Bad dword read at : " << 0x402000;
}

TEST_F(Unit_ByteMapF, legacy_qword)
{
    this->byteMap.setData(gtirb::EA(0x403000), uint64_t(0x8BADF00D0D15EA5E));
    EXPECT_EQ(uint64_t(0x8BADF00D0D15EA5E), this->byteMap.getData64(gtirb::EA(0x403000)))
        << "Bad qword read at : " << 0x403000;
}

TEST_F(Unit_ByteMapF, legacy_boundariesWithZero_0)
{
    // Boundaries w/ 0's.
    const auto ea = Unit_ByteMapF::Offset - gtirb::EA{16};
    const auto buf = this->byteMap.getData(ea, 32);

    for(size_t i = 0; i < 32; ++i)
    {
        if(i < 16)
        {
            EXPECT_EQ(0, buf[i]) << "Bad chunk read at : " << ea << " plus : " << i;
        }
        else
        {
            EXPECT_EQ((this->InitialByte & (i - 16)), buf[i]) << "Bad chunk read at : " << ea
                                                              << " plus : " << i;
        }
    }
}

TEST_F(Unit_ByteMapF, legacy_boundariesWithZero_1)
{
    // Boundaries w/ 0's.
    const auto ea = Unit_ByteMapF::Offset + gtirb::EA{this->InitializedSize} - gtirb::EA{16};
    const auto buf = this->byteMap.getData(ea, 32);

    for(size_t i = 0; i < 32; ++i)
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

TEST_F(Unit_ByteMapF, legacy_sentinelSearch_0)
{
    // a. search for 0 -- should be at start of next page
    const auto ea = Unit_ByteMapF::Offset + gtirb::EA{InitializedSize} - gtirb::EA{16};
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

TEST_F(Unit_ByteMapF, legacy_sentinelSearch_1)
{
    // b. search for 2 -- not found, should reach limit (32)
    const auto ea = Unit_ByteMapF::Offset + gtirb::EA{InitializedSize} - gtirb::EA{32};
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

TEST_F(Unit_ByteMapF, legacy_sentinelSearch_2)
{
    // c. search for 254 -- like a, but two fewer
    const auto ea = Unit_ByteMapF::Offset + gtirb::EA{InitializedSize} - gtirb::EA{16};
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

TEST_F(Unit_ByteMapF, serialize)
{
    const auto tempPath =
        boost::filesystem::temp_directory_path() / boost::filesystem::unique_path();
    const std::string tempPathString = tempPath.string();

    // Scope objects so they are destroyed
    {
        // Serialize Out.
        std::ofstream ofs{tempPathString.c_str()};
        boost::archive::polymorphic_text_oarchive oa{ofs};
        EXPECT_TRUE(ofs.is_open());

        EXPECT_NO_THROW(oa << this->byteMap);

        EXPECT_NO_THROW(ofs.close());
        EXPECT_FALSE(ofs.is_open());
    }

    // Read it back in and re-test
    {
        gtirb::ByteMap serialized;

        // Serialize In.
        std::ifstream ifs{tempPathString.c_str()};
        boost::archive::polymorphic_text_iarchive ia{ifs};

        EXPECT_NO_THROW(ia >> serialized);

        EXPECT_NO_THROW(ifs.close());

        // Copy-pasted test from above
        {
            // Boundaries w/ 0's.
            const auto ea =
                Unit_ByteMapF::Offset + gtirb::EA{this->InitializedSize} - gtirb::EA{16};
            const auto buf = serialized.getData(ea, 32);

            for(size_t i = 0; i < 32; ++i)
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

        // Copy-pasted test from above
        {
            for(size_t i = 0; i < this->InitializedSize; ++i)
            {
                const auto expectedWord = (((InitialByte & i) | ((InitialByte & (i + 1)) << 8)));

                EXPECT_EQ(this->InitialByte & i,
                          serialized.getData8(Unit_ByteMapF::Offset
                                              + gtirb::EA{static_cast<uint64_t>(i)}))
                    << "Bad byte read at : " << Unit_ByteMapF::Offset + gtirb::EA{i};

                if(i < this->InitializedSize - 1)
                {
                    EXPECT_NO_THROW(serialized.getData(Unit_ByteMapF::Offset + gtirb::EA{i}, 2));

                    const auto word = serialized.getData16(Unit_ByteMapF::Offset + gtirb::EA{i});

                    EXPECT_EQ(expectedWord, word) << "Bad word read at : "
                                                  << Unit_ByteMapF::Offset + gtirb::EA{i};
                }
            }
        }
    }
}
