#include <gtest/gtest.h>
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
