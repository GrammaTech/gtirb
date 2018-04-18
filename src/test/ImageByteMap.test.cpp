#include <gtest/gtest.h>
#include <gtirb/Constants.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Utilities.hpp>
#include <memory>

class Unit_ImageByteMapF : public ::testing::Test
{
public:
    virtual void SetUp() override
    {
        for(size_t i = 0; i < this->InitializedSize; ++i)
        {
            this->byteMap.setData(Unit_ImageByteMapF::Offset + gtirb::EA{i}, static_cast<uint8_t>(this->InitialByte & i));
        }
    }

    static constexpr uint8_t InitialByte{0xFF};
    static gtirb::EA Offset;
    static constexpr size_t InitializedSize{gtirb::constants::PageSize * 2};

    gtirb::ImageByteMap byteMap{};
};

gtirb::EA Unit_ImageByteMapF::Offset{static_cast<uint64_t>(gtirb::constants::PageSize)};

TEST(Unit_ImageByteMap, ctor_0)
{
    EXPECT_NO_THROW(gtirb::ImageByteMap());
}

// Test ported from libswyx's test for fimage_this->byteMap
TEST_F(Unit_ImageByteMapF, legacy_1)
{
    for(size_t i = 0; i < this->InitializedSize; ++i)
    {
        const auto expectedWord = (((InitialByte & i) | ((InitialByte & (i+1)) << 8)));

        EXPECT_EQ(this->InitialByte & i, this->byteMap.getData(Unit_ImageByteMapF::Offset + gtirb::EA{static_cast<uint64_t>(i)}))
            << "Bad byte read at : " << Unit_ImageByteMapF::Offset + gtirb::EA{i};

        if(i < this->InitializedSize - 1)
        {
            EXPECT_NO_THROW(this->byteMap.getData(Unit_ImageByteMapF::Offset + gtirb::EA{i}, 2));

            const auto byteArray = this->byteMap.getData(Unit_ImageByteMapF::Offset + gtirb::EA{i}, 2);
            const auto wordArray = gtirb::utilities::ByteArray8To16(byteArray, false);
            const auto word = wordArray[0];

            EXPECT_EQ(expectedWord, word)
                << "Bad word read at : " << Unit_ImageByteMapF::Offset + gtirb::EA{i};
        }
    }
}

/*
    w_this->byteMap.set_uint16(gtirb::EA(0x401000), 0xDEAD);
    if(this->byteMap.get_uint16(gtirb::EA(0x401000)) != 0xDEAD)
    {
        std::cerr << "Bad word read at : " << 0x401000 << std::endl;
        return 3;
    }

    w_this->byteMap.set_uint32(gtirb::EA(0x402000), 0xCAFEBABE);
    if(this->byteMap.get_uint32(gtirb::EA(0x402000)) != 0xCAFEBABE)
    {
        std::cerr << "Bad dword read at : " << 0x402000 << std::endl;
        return 4;
    }

    w_this->byteMap.set_uint64(gtirb::EA(0x403000), CSUINT64_LIT(0x8BADF00D0D15EA5E));
    if(this->byteMap.get_uint64(gtirb::EA(0x403000)) != CSUINT64_LIT(0x8BADF00D0D15EA5E))
    {
        std::cerr << "Bad qword read at : " << 0x403000 << std::endl;
        return 5;
    }

    // Boundaries w/ 0's.
    uint8_t buf[32];
    this->byteMap.get_byte_chunk(Unit_ImageByteMapF::Offset - 16, buf, 32);

    for(size_t i = 0; i < 32; ++i)
    {
        if(i < 16 && buf[i] != 0)
        {
            std::cerr << "Bad chunk read at : " << Unit_ImageByteMapF::Offset - 16 << " plus : " << i <<
std::endl;
            return 6;
        }
        else if(i >= 16 && buf[i] != (this->InitialByte & (i - 16)))
        {
            std::cerr << "Bad chunk read at : " << Unit_ImageByteMapF::Offset - 16 << " plus : " << i <<
std::endl;
            return 7;
        }
    }

    uint8_t buf2[32];
    this->byteMap.get_byte_chunk(Unit_ImageByteMapF::Offset + InitializedSize - 16, buf2, 32);

    for(size_t i = 0; i < 32; ++i)
    {
        if(i >= 16 && buf2[i] != 0)
        {
            std::cerr << "Bad chunk read at : " << Unit_ImageByteMapF::Offset + InitializedSize - 16 << " plus : "
<< i
                      << std::endl;
            return 8;
        }
        else if(i < 16 && buf2[i] != (this->InitialByte & (i + InitializedSize - 16)))
        {
            std::cerr << "Bad chunk read at : " << Unit_ImageByteMapF::Offset + InitializedSize - 16 << " plus : "
<< i
                      << std::endl;
            return 9;
        }
    }

    // Sentinel search
    // a. search for 0 -- should be at start of next page
    uint8_t buf3[32];
    size_t nread3 = this->byteMap.get_byte_chunk_until(Unit_ImageByteMapF::Offset + InitializedSize - 16, '\0',
buf3, 32);

    if(nread3 != 17)
    {
        std::cerr << "Expecting 17, read " << nread3 << std::endl;
        return 10;
    }
    for(size_t i = 0; i < nread3; ++i)
    {
        if(i >= 16 && buf3[i] != 0)
        {
            std::cerr << "Bad chunk read at : " << Unit_ImageByteMapF::Offset + InitializedSize - 16 << " plus : "
<< i
                      << std::endl;
            return 11;
        }
        else if(i < 16 && buf3[i] != (this->InitialByte & (i + InitializedSize - 16)))
        {
            std::cerr << "Bad chunk read at : " << Unit_ImageByteMapF::Offset + InitializedSize - 16 << " plus : "
<< i
                      << std::endl;
            return 12;
        }
    }

    // b. search for 2 -- not found, should reach limit (32)
    uint8_t buf4[32];
    size_t nread4 = this->byteMap.get_byte_chunk_until(Unit_ImageByteMapF::Offset + InitializedSize - 16, '\x02',
buf4, 32);

    if(nread4 != 32)
    {
        std::cerr << "Expecting 32, read " << nread4 << std::endl;
        return 10;
    }

    for(size_t i = 0; i < nread4; ++i)
    {
        if(i >= 16 && buf4[i] != 0)
        {
            std::cerr << "Bad chunk read at : " << Unit_ImageByteMapF::Offset + InitializedSize - 16 << " plus : "
<< i
                      << std::endl;
            return 13;
        }
        else if(i < 16 && buf4[i] != (this->InitialByte & (i + InitializedSize - 16)))
        {
            std::cerr << "Bad chunk read at : " << Unit_ImageByteMapF::Offset + InitializedSize - 16 << " plus : "
<< i
                      << std::endl;
            return 14;
        }
    }

    // c. search for 254 -- like a, but two fewer
    uint8_t buf5[32];
    size_t nread5 = this->byteMap.get_byte_chunk_until(Unit_ImageByteMapF::Offset + InitializedSize - 16, '\xfe',
buf5, 32);

    if(nread5 != 15)
    {
        std::cerr << "Expecting 15, read " << nread5 << std::endl;
        return 10;
    }
    for(size_t i = 0; i < nread5; ++i)
    {
        if(i >= 16 && buf5[i] != 0)
        {
            std::cerr << "Bad chunk read at : " << Unit_ImageByteMapF::Offset + InitializedSize - 16 << " plus : "
<< i
                      << std::endl;
            return 15;
        }
        else if(i < 16 && buf5[i] != (this->InitialByte & (i + InitializedSize - 16)))
        {
            std::cerr << "Bad chunk read at : " << Unit_ImageByteMapF::Offset + InitializedSize - 16 << " plus : "
<< i
                      << std::endl;
            return 16;
        }
    }
}
*/