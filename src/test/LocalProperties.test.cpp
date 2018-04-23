#include <gtest/gtest.h>
#include <boost/archive/polymorphic_text_iarchive.hpp>
#include <boost/archive/polymorphic_text_oarchive.hpp>
#include <boost/filesystem.hpp>
#include <boost/serialization/shared_ptr.hpp>
#include <boost/serialization/shared_ptr_helper.hpp>
#include <fstream>
#include <gtirb/Node.hpp>
#include <memory>

TEST(Unit_LocalProperties, ctor_0)
{
    EXPECT_NO_THROW(gtirb::LocalProperties());
}

TEST(Unit_LocalProperties, setLocalProperty)
{
    auto test = gtirb::LocalProperties();

    EXPECT_TRUE(test.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), test.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp{"Foo", "Bar"};
    EXPECT_NO_THROW(test.setLocalProperty(nvp.first, nvp.second));
    EXPECT_FALSE(test.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), test.getLocalPropertySize());
}

TEST(Unit_LocalProperties, setLocalPropertyReset)
{
    auto test = gtirb::LocalProperties();

    EXPECT_TRUE(test.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), test.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp1{"Foo", "Bar"};
    const std::pair<std::string, std::string> nvp2{"Foo", "Bah"};

    EXPECT_NO_THROW(test.setLocalProperty(nvp1.first, nvp1.second));
    EXPECT_FALSE(test.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), test.getLocalPropertySize());

    EXPECT_NO_THROW(test.setLocalProperty(nvp2.first, nvp2.second));
    EXPECT_FALSE(test.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), test.getLocalPropertySize());
}

TEST(Unit_LocalProperties, setLocalProperties)
{
    auto test = gtirb::LocalProperties();

    EXPECT_TRUE(test.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), test.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp1{"Foo", "Bar"};
    const std::pair<std::string, std::string> nvp2{"Bar", "Foo"};

    EXPECT_NO_THROW(test.setLocalProperty(nvp1.first, nvp1.second));
    EXPECT_FALSE(test.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), test.getLocalPropertySize());

    EXPECT_NO_THROW(test.setLocalProperty(nvp2.first, nvp2.second));
    EXPECT_FALSE(test.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(2), test.getLocalPropertySize());
}

TEST(Unit_LocalProperties, removeLocalProperty)
{
    auto test = gtirb::LocalProperties();

    EXPECT_TRUE(test.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), test.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp1{"Foo", "Bar"};
    const std::pair<std::string, std::string> nvp2{"Bar", "Foo"};

    EXPECT_NO_THROW(test.setLocalProperty(nvp1.first, nvp1.second));
    EXPECT_FALSE(test.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), test.getLocalPropertySize());

    EXPECT_NO_THROW(test.setLocalProperty(nvp2.first, nvp2.second));
    EXPECT_FALSE(test.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(2), test.getLocalPropertySize());

    EXPECT_TRUE(test.removeLocalProperty("Foo"));
    EXPECT_FALSE(test.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), test.getLocalPropertySize());

    EXPECT_TRUE(test.removeLocalProperty("Bar"));
    EXPECT_TRUE(test.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), test.getLocalPropertySize());
}

TEST(Unit_LocalProperties, clearLocalProperties)
{
    auto test = gtirb::LocalProperties();

    EXPECT_TRUE(test.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), test.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp1{"Foo", "Bar"};
    const std::pair<std::string, std::string> nvp2{"Bar", "Foo"};

    EXPECT_NO_THROW(test.setLocalProperty(nvp1.first, nvp1.second));
    EXPECT_FALSE(test.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), test.getLocalPropertySize());

    EXPECT_NO_THROW(test.setLocalProperty(nvp2.first, nvp2.second));
    EXPECT_FALSE(test.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(2), test.getLocalPropertySize());

    EXPECT_NO_THROW(test.clearLocalProperties());

    EXPECT_TRUE(test.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), test.getLocalPropertySize());
}

TEST(Unit_LocalProperties, serialize)
{
    const auto tempPath =
        boost::filesystem::temp_directory_path() / boost::filesystem::unique_path();
    const std::string tempPathString = tempPath.string();

    auto original = gtirb::LocalProperties{};
    original.setLocalProperty("Name", std::string("Value"));

    // Scope objects so they are destroyed
    {
        EXPECT_EQ(size_t{1}, original.getLocalPropertySize());
        EXPECT_EQ(std::string{"Value"}, boost::get<std::string>(original.getLocalProperty("Name")));

        // Serialize Out.
        std::ofstream ofs{tempPathString.c_str()};
        boost::archive::polymorphic_text_oarchive oa{ofs};
        EXPECT_TRUE(ofs.is_open());

        oa << original;

        EXPECT_NO_THROW(ofs.close());
        EXPECT_FALSE(ofs.is_open());
    }

    // Read it back in and re-test
    {
        gtirb::LocalProperties serialized;

        // Serialize In.
        std::ifstream ifs{tempPathString.c_str()};
        boost::archive::polymorphic_text_iarchive ia{ifs};

        ia >> serialized;

        EXPECT_NO_THROW(ifs.close());

        EXPECT_EQ(size_t{1}, serialized.getLocalPropertySize());
        EXPECT_EQ(std::string{"Value"},
                  boost::get<std::string>(serialized.getLocalProperty("Name")));
    }
}

TEST(Unit_LocalProperties, serializeFromSharedPtr)
{
    const auto tempPath =
        boost::filesystem::temp_directory_path() / boost::filesystem::unique_path();
    const std::string tempPathString = tempPath.string();

    auto original = std::make_shared<gtirb::LocalProperties>();
    original->setLocalProperty("Name", std::string("Value"));

    // Scope objects so they are destroyed
    {
        EXPECT_EQ(size_t{1}, original->getLocalPropertySize());
        EXPECT_EQ(std::string{"Value"},
                  boost::get<std::string>(original->getLocalProperty("Name")));

        // Serialize Out.
        std::ofstream ofs{tempPathString.c_str()};
        boost::archive::polymorphic_text_oarchive oa{ofs};
        EXPECT_TRUE(ofs.is_open());

        oa << original;

        EXPECT_NO_THROW(ofs.close());
        EXPECT_FALSE(ofs.is_open());
    }

    // Read it back in and re-test
    {
        std::shared_ptr<gtirb::LocalProperties> serialized =
            std::make_shared<gtirb::LocalProperties>();

        // Serialize In.
        std::ifstream ifs{tempPathString.c_str()};
        boost::archive::polymorphic_text_iarchive ia{ifs};

        ia >> serialized;

        EXPECT_NO_THROW(ifs.close());

        EXPECT_EQ(size_t{1}, serialized->getLocalPropertySize());
        EXPECT_EQ(std::string{"Value"},
                  boost::get<std::string>(serialized->getLocalProperty("Name")));
    }
}
