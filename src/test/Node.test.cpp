#include <gtest/gtest.h>
#include <boost/archive/polymorphic_text_iarchive.hpp>
#include <boost/archive/polymorphic_text_oarchive.hpp>
#include <boost/filesystem.hpp>
#include <boost/serialization/shared_ptr.hpp>
#include <boost/serialization/shared_ptr_helper.hpp>
#include <fstream>
#include <gtirb/Node.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <memory>

TEST(Unit_Node, ctor_0)
{
    EXPECT_NO_THROW(gtirb::Node());
}

TEST(Unit_Node, uniqueUuids)
{
    std::vector<std::string> uuids;

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

TEST(Unit_Node, push_backFailure)
{
    auto node = gtirb::Node();

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());

    auto nodeDuplicate = std::make_unique<gtirb::Node>(node);

    // We cannot become a parent to ourself.
    // While this isn't the same object, it will have the same UUID, so that's good enough.
    EXPECT_THROW(node.push_back(std::move(nodeDuplicate)), gtirb::NodeStructureError);

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());
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
        EXPECT_EQ(size_t(i + 1), node.size());
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
        EXPECT_EQ(size_t(i + 1), node.size());
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
        EXPECT_EQ(size_t(i + 1), node.size());
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
        EXPECT_EQ(size_t(i + 1), node.size());
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

TEST(Unit_Node, serialize)
{
    const auto tempPath =
        boost::filesystem::temp_directory_path() / boost::filesystem::unique_path();
    const std::string tempPathString = tempPath.string();

    auto original = gtirb::Node{};
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
        gtirb::Node serialized;

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

TEST(Unit_Node, serializeFromSharedPtr)
{
    const auto tempPath =
        boost::filesystem::temp_directory_path() / boost::filesystem::unique_path();
    const std::string tempPathString = tempPath.string();

    auto original = std::make_shared<gtirb::Node>();
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
        std::shared_ptr<gtirb::Node> serialized = std::make_shared<gtirb::Node>();

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

TEST(Unit_Node, serializeFromSharedPtrWChildren)
{
    const auto tempPath =
        boost::filesystem::temp_directory_path() / boost::filesystem::unique_path();
    const std::string tempPathString = tempPath.string();

    auto original = std::make_shared<gtirb::Node>();
    original->setLocalProperty("Name", std::string("Value"));

    auto foo = std::make_unique<gtirb::Node>();
    foo->setLocalProperty("Name", std::string("Foo"));
    original->push_back(std::move(foo));

    auto bar = std::make_unique<gtirb::Node>();
    bar->setLocalProperty("Name", std::string("Bar"));
    original->push_back(std::move(bar));

    EXPECT_EQ(size_t{2}, original->size());

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
        std::shared_ptr<gtirb::Node> serialized = std::make_shared<gtirb::Node>();

        // Serialize In.
        std::ifstream ifs{tempPathString.c_str()};
        boost::archive::polymorphic_text_iarchive ia{ifs};

        ia >> serialized;

        EXPECT_NO_THROW(ifs.close());

        EXPECT_EQ(size_t{1}, serialized->getLocalPropertySize());
        EXPECT_EQ(std::string{"Value"},
                  boost::get<std::string>(serialized->getLocalProperty("Name")));

        ASSERT_EQ(size_t{2}, serialized->size());

        auto child0 = serialized->at(0);
        EXPECT_EQ(std::string{"Foo"}, boost::get<std::string>(child0->getLocalProperty("Name")));
        EXPECT_EQ(serialized.get(), child0->getNodeParent());

        auto child1 = serialized->at(1);
        EXPECT_EQ(std::string{"Bar"}, boost::get<std::string>(child1->getLocalProperty("Name")));
        EXPECT_EQ(serialized.get(), child1->getNodeParent());
    }
}
