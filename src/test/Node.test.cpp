#include <gtest/gtest.h>
#include <boost/archive/polymorphic_text_iarchive.hpp>
#include <boost/archive/polymorphic_text_oarchive.hpp>
#include <boost/filesystem.hpp>
#include <boost/serialization/shared_ptr.hpp>
#include <boost/serialization/shared_ptr_helper.hpp>
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

TEST(Unit_Node, deserializeUpdatesUUIDMap)
{
    std::stringstream out;
    gtirb::UUID id;

    {
        gtirb::Node node;
        id = node.getUUID();

        boost::archive::polymorphic_text_oarchive oa{out};
        oa << node;
    }

    EXPECT_EQ(gtirb::Node::getByUUID(id), nullptr);

    std::istringstream in(out.str());
    boost::archive::polymorphic_text_iarchive ia{out};
    gtirb::Node newNode;
    ia >> newNode;

    EXPECT_EQ(gtirb::Node::getByUUID(id), &newNode);
}

TEST(Unit_Node, symbolReference)
{
    gtirb::Node sym;
    gtirb::NodeReference<gtirb::Node> ref(sym);

    gtirb::Node *ptr = ref;
    EXPECT_EQ(ptr, &sym);
    EXPECT_EQ(ref->getUUID(), sym.getUUID());
}

TEST(Unit_Node, badReference)
{
    gtirb::Node sym;
    gtirb::NodeReference<gtirb::Node> ref(gtirb::UUID{});

    gtirb::Node *ptr = ref;
    EXPECT_EQ(ptr, nullptr);
}

TEST(Unit_Node, serializeNodeReference)
{
    gtirb::Node sym;
    gtirb::NodeReference<gtirb::Node> ref(sym);

    std::stringstream out;
    boost::archive::polymorphic_text_oarchive oa{out};
    oa << ref;

    std::istringstream in(out.str());
    boost::archive::polymorphic_text_iarchive ia{out};
    gtirb::NodeReference<gtirb::Node> ref2;
    ia >> ref2;

    EXPECT_EQ(&*ref2, &*ref);
}
