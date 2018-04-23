#include <gtest/gtest.h>
#include <boost/archive/polymorphic_text_iarchive.hpp>
#include <boost/archive/polymorphic_text_oarchive.hpp>
#include <boost/filesystem.hpp>
#include <fstream>
#include <gtirb/EA.hpp>
#include <memory>

TEST(Unit_EA, ctor_0)
{
    EXPECT_NO_THROW(gtirb::EA());
    EXPECT_EQ(gtirb::EA(gtirb::constants::BadAddress), gtirb::EA());
}

TEST(Unit_EA, ctor_1)
{
    EXPECT_NO_THROW(gtirb::EA(2112));

    auto ea = gtirb::EA(2112);
    EXPECT_EQ(uint64_t(2112), ea.get());
}

TEST(Unit_EA, comparison)
{
    auto ea1 = gtirb::EA(2112);
    auto ea2 = gtirb::EA(1221);

    EXPECT_GT(ea1, ea2);
    EXPECT_TRUE(ea1 > ea2);

    EXPECT_NE(ea1, ea2);
    EXPECT_TRUE(ea1 != ea2);

    EXPECT_LT(ea2, ea1);
    EXPECT_TRUE(ea2 < ea1);

    EXPECT_FALSE(ea1 == ea2);
}

TEST(Unit_EA, set)
{
    auto ea1 = gtirb::EA(2112);
    auto ea2 = gtirb::EA();

    ea2.set(uint64_t(2112));

    EXPECT_EQ(ea1, ea2);
    EXPECT_EQ(uint64_t(2112), ea2.get());
}

TEST(Unit_EA, serialize)
{
    const auto tempPath =
        boost::filesystem::temp_directory_path() / boost::filesystem::unique_path();
    const std::string tempPathString = tempPath.string();

    const auto original = gtirb::EA{2112};

    // Scope objects so they are destroyed
    {
        EXPECT_EQ(gtirb::EA{2112}, original);

        // Serialize Out.
        std::ofstream ofs{tempPathString.c_str()};
        boost::archive::polymorphic_text_oarchive oa{ofs};
        EXPECT_TRUE(ofs.is_open());

        oa << original;

        EXPECT_NO_THROW(ofs.close());
        EXPECT_FALSE(ofs.is_open());
    }

    const auto fileSize = boost::filesystem::file_size(tempPath);
    EXPECT_EQ(size_t(37), fileSize);

    // Read it back in and re-test
    {
        gtirb::EA serialized;

        // Serialize In.
        std::ifstream ifs{tempPathString.c_str()};
        boost::archive::polymorphic_text_iarchive ia{ifs};

        ia >> serialized;

        EXPECT_NO_THROW(ifs.close());

        EXPECT_EQ(original, serialized);
    }
}
