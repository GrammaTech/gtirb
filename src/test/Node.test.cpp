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

TEST(Unit_Node, copyGetsNewUUID)
{
    gtirb::Node node;
    gtirb::Node copy(node);

    EXPECT_NE(node.getUUID(), copy.getUUID());
    EXPECT_EQ(gtirb::Node::getByUUID(node.getUUID()), &node);
    EXPECT_EQ(gtirb::Node::getByUUID(copy.getUUID()), &copy);
}
