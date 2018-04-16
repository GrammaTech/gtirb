#include <gtest/gtest.h>
#include <gtirb/Node.hpp>
#include <memory>

TEST(Unit_Node, ctor_0)
{
    EXPECT_NO_THROW(gtirb::Node());
}

TEST(Unit_Node, uniqueUuids)
{
    std::vector<boost::uuids::uuid> uuids;

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

TEST(Unit_Node, push_back)
{
    auto node = gtirb::Node();

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());

    EXPECT_NO_THROW(node.push_back(std::make_unique<gtirb::Node>()));

    EXPECT_FALSE(node.empty());
    EXPECT_EQ(size_t(1), node.size());
}

TEST(Unit_Node, size)
{
    const int totalChildren = 512;
    auto node = gtirb::Node();

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());

    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_NO_THROW(node.push_back(std::make_unique<gtirb::Node>()));
        EXPECT_FALSE(node.empty());
        EXPECT_EQ(size_t(i+1), node.size());
    }
}

TEST(Unit_Node, clear)
{
    const int totalChildren = 512;
    auto node = gtirb::Node();

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());

    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_NO_THROW(node.push_back(std::make_unique<gtirb::Node>()));
        EXPECT_FALSE(node.empty());
        EXPECT_EQ(size_t(i+1), node.size());
    }

    EXPECT_NO_THROW(node.clear());

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());
}

TEST(Unit_Node, iterator)
{
    const int totalChildren = 8;

    auto node = gtirb::Node();

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());

    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_NO_THROW(node.push_back(std::make_unique<gtirb::Node>()));
        EXPECT_FALSE(node.empty());
        EXPECT_EQ(size_t(i+1), node.size());
    }

    // We should now be able to use our begin iterator "totalChildren" times.
    auto nodeIterator = std::begin(node);
    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_TRUE(nodeIterator != std::end(node));
        
        EXPECT_EQ(node.at(i)->getUUID(), nodeIterator->getUUID());
        ++nodeIterator;

        EXPECT_NE(nodeIterator, std::begin(node));
    }

    EXPECT_NE(nodeIterator, std::begin(node));
    EXPECT_EQ(nodeIterator, std::end(node));
}


TEST(Unit_Node, const_iterator)
{
    const int totalChildren = 8;

    auto node = gtirb::Node();

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());

    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_NO_THROW(node.push_back(std::make_unique<gtirb::Node>()));
        EXPECT_FALSE(node.empty());
        EXPECT_EQ(size_t(i+1), node.size());
    }

    // Make a const node by copy construction.
    const auto constNode{node};

    // We should now be able to use our begin iterator "totalChildren" times.
    auto nodeIterator = std::begin(constNode);
    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_TRUE(nodeIterator != std::end(constNode));
        
        EXPECT_EQ(constNode.at(i)->getUUID(), nodeIterator->getUUID());
        ++nodeIterator;

        EXPECT_NE(nodeIterator, std::begin(constNode));
    }

    EXPECT_NE(nodeIterator, std::begin(constNode));
    EXPECT_EQ(nodeIterator, std::end(constNode));
}
