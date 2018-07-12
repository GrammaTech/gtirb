#include <gtest/gtest.h>
#include <boost/archive/polymorphic_text_iarchive.hpp>
#include <boost/archive/polymorphic_text_oarchive.hpp>
#include <boost/filesystem.hpp>
#include <fstream>
#include <gtirb/Node.hpp>
#include <gtirb/NodeReference.hpp>
#include <memory>

TEST(Unit_Node, ctor_0)
{
    EXPECT_NO_THROW(gtirb::Node());
}

TEST(Unit_Node, uniqueUuids)
{
    std::vector<gtirb::UUID> uuids;

    // Create a bunch of UUID's, then make sure we don't have any duplicates.

    for(size_t i = 0; i < 512; ++i)
    {
        const auto n = gtirb::Node();
        uuids.push_back(n.getUUID());
    }

    std::sort(std::begin(uuids), std::end(uuids));
    const auto end = std::unique(std::begin(uuids), std::end(uuids));

    EXPECT_EQ(std::end(uuids), end) << "Duplicate UUID's were generated.";
}

TEST(Unit_Node, setLocalProperty)
{
    auto node = gtirb::Node();

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp{"Foo", "Bar"};
    EXPECT_NO_THROW(node.setLocalProperty(nvp.first, nvp.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());
}

TEST(Unit_Node, setLocalPropertyReset)
{
    auto node = gtirb::Node();

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp1{"Foo", "Bar"};
    const std::pair<std::string, std::string> nvp2{"Foo", "Bah"};

    EXPECT_NO_THROW(node.setLocalProperty(nvp1.first, nvp1.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());

    EXPECT_NO_THROW(node.setLocalProperty(nvp2.first, nvp2.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());
}

TEST(Unit_Node, setLocalProperties)
{
    auto node = gtirb::Node();

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp1{"Foo", "Bar"};
    const std::pair<std::string, std::string> nvp2{"Bar", "Foo"};

    EXPECT_NO_THROW(node.setLocalProperty(nvp1.first, nvp1.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());

    EXPECT_NO_THROW(node.setLocalProperty(nvp2.first, nvp2.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(2), node.getLocalPropertySize());
}

TEST(Unit_Node, removeLocalProperty)
{
    auto node = gtirb::Node();

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp1{"Foo", "Bar"};
    const std::pair<std::string, std::string> nvp2{"Bar", "Foo"};

    EXPECT_NO_THROW(node.setLocalProperty(nvp1.first, nvp1.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());

    EXPECT_NO_THROW(node.setLocalProperty(nvp2.first, nvp2.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(2), node.getLocalPropertySize());

    EXPECT_TRUE(node.removeLocalProperty("Foo"));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());

    EXPECT_TRUE(node.removeLocalProperty("Bar"));
    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());
}

TEST(Unit_Node, clearLocalProperties)
{
    auto node = gtirb::Node();

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp1{"Foo", "Bar"};
    const std::pair<std::string, std::string> nvp2{"Bar", "Foo"};

    EXPECT_NO_THROW(node.setLocalProperty(nvp1.first, nvp1.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());

    EXPECT_NO_THROW(node.setLocalProperty(nvp2.first, nvp2.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(2), node.getLocalPropertySize());

    EXPECT_NO_THROW(node.clearLocalProperties());

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());
}

TEST(Unit_node, getByUUID)
{
    gtirb::Node node;
    EXPECT_EQ(gtirb::Node::getByUUID(node.getUUID()), &node);
}

TEST(Unit_node, setUUIDUpdatesUUIDMap)
{
    gtirb::Node node;
    gtirb::UUID oldId(node.getUUID());
    gtirb::UUID newId;

    node.setUUID(newId);

    EXPECT_EQ(gtirb::Node::getByUUID(newId), &node);
    EXPECT_EQ(gtirb::Node::getByUUID(oldId), nullptr);
}
