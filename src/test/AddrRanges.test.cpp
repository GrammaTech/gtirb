#include <gtest/gtest.h>
#include <proto/AddrRanges.pb.h>
#include <boost/archive/polymorphic_text_iarchive.hpp>
#include <boost/archive/polymorphic_text_oarchive.hpp>
#include <boost/filesystem.hpp>
#include <gtirb/AddrRanges.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/RuntimeError.hpp>
#include <memory>

TEST(Unit_AddrRanges, ctor_0)
{
    EXPECT_NO_THROW(gtirb::AddrRanges());
}

TEST(Unit_AddrRanges, validRange)
{
    auto node = std::make_unique<gtirb::AddrRanges>();

    EXPECT_TRUE(node->data().empty());
    EXPECT_NO_THROW(node->addRange({gtirb::EA{1221}, gtirb::EA{2112}}));
    EXPECT_FALSE(node->data().empty());
    EXPECT_EQ(size_t{1}, node->data().size());
}

TEST(Unit_AddrRanges, invalidRange)
{
    auto node = std::make_unique<gtirb::AddrRanges>();

    EXPECT_TRUE(node->data().empty());
    EXPECT_THROW(node->addRange({gtirb::EA{2112}, gtirb::EA{1221}}), gtirb::RuntimeError);
    EXPECT_TRUE(node->data().empty());
    EXPECT_EQ(size_t{0}, node->data().size());
}

TEST(Unit_AddrRanges, grow)
{
    gtirb::AddrRanges ar;

    EXPECT_TRUE(ar.data().empty());
    EXPECT_EQ(size_t{0}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{7}, gtirb::EA{8}}));
    EXPECT_EQ(size_t{1}, ar.data().size());
    EXPECT_EQ(size_t{1}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{3}, gtirb::EA{4}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{2}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{4}, gtirb::EA{5}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{3}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{6}, gtirb::EA{7}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{4}, ar.getBytesCoveredByRanges());
}

TEST(Unit_AddrRanges, clearRanges)
{
    gtirb::AddrRanges ar;

    EXPECT_TRUE(ar.data().empty());
    EXPECT_EQ(size_t{0}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{7}, gtirb::EA{8}}));
    EXPECT_EQ(size_t{1}, ar.data().size());
    EXPECT_EQ(size_t{1}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{3}, gtirb::EA{4}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{2}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{4}, gtirb::EA{5}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{3}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{6}, gtirb::EA{7}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{4}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.clearRanges());
    EXPECT_TRUE(ar.data().empty());
    EXPECT_EQ(size_t{0}, ar.getBytesCoveredByRanges());
}

TEST(Unit_AddrRanges, range)
{
    gtirb::AddrRanges ar;

    EXPECT_TRUE(ar.data().empty());
    EXPECT_EQ(size_t{0}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{70}, gtirb::EA{80}}));
    EXPECT_EQ(size_t{1}, ar.data().size());
    EXPECT_EQ(size_t{10}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{30}, gtirb::EA{40}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{20}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{40}, gtirb::EA{50}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{30}, ar.getBytesCoveredByRanges());

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{60}, gtirb::EA{70}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{40}, ar.getBytesCoveredByRanges());

    // Print additional debugging information
    if(ar.data().size() != 2)
    {
        for(auto& range : ar.data())
        {
            ADD_FAILURE() << "Range {" << range.first << ", " << range.second << "}";
        }
    }

    EXPECT_NO_THROW(ar.addRange({gtirb::EA{50}, gtirb::EA{60}}));
    EXPECT_EQ(size_t{1}, ar.data().size());
    EXPECT_EQ(size_t{50}, ar.getBytesCoveredByRanges());
}

TEST(Unit_AddrRanges, subtractRange)
{
    gtirb::AddrRanges ar;

    EXPECT_TRUE(ar.data().empty());
    EXPECT_EQ(size_t{0}, ar.getBytesCoveredByRanges());

    EXPECT_TRUE(ar.addRange({gtirb::EA{70}, gtirb::EA{80}}));
    EXPECT_EQ(size_t{1}, ar.data().size());
    EXPECT_EQ(size_t{10}, ar.getBytesCoveredByRanges());

    EXPECT_TRUE(ar.addRange({gtirb::EA{30}, gtirb::EA{40}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{20}, ar.getBytesCoveredByRanges());

    EXPECT_TRUE(ar.subtractRange({gtirb::EA{31}, gtirb::EA{36}}));
    EXPECT_EQ(size_t{3}, ar.data().size());
    EXPECT_EQ(size_t{15}, ar.getBytesCoveredByRanges());

    EXPECT_TRUE(ar.addRange({gtirb::EA{40}, gtirb::EA{50}}));
    EXPECT_EQ(size_t{3}, ar.data().size());
    EXPECT_EQ(size_t{25}, ar.getBytesCoveredByRanges());

    EXPECT_TRUE(ar.addRange({gtirb::EA{60}, gtirb::EA{70}}));
    EXPECT_EQ(size_t{3}, ar.data().size());
    EXPECT_EQ(size_t{35}, ar.getBytesCoveredByRanges());

    EXPECT_TRUE(ar.addRange({gtirb::EA{50}, gtirb::EA{60}}));
    EXPECT_EQ(size_t{2}, ar.data().size());
    EXPECT_EQ(size_t{45}, ar.getBytesCoveredByRanges());
}

TEST(Unit_AddrRanges, protobufRoundTrip)
{
    gtirb::AddrRanges ranges1;
    ranges1.addRange({gtirb::EA(1), gtirb::EA(2)});
    ranges1.addRange({gtirb::EA(6), gtirb::EA(7)});

    gtirb::AddrRanges ranges2;
    proto::AddrRanges message;
    ranges1.toProtobuf(&message);
    ranges2.fromProtobuf(message);

    EXPECT_EQ(ranges1.data(), ranges2.data());
}
