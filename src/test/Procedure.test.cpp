#include <gtest/gtest.h>
#include <proto/Procedure.pb.h>
#include <gtirb/Procedure.hpp>
#include <memory>

using namespace gtirb;

TEST(Unit_Procedure, ctor_0)
{
    EXPECT_NO_THROW(gtirb::Procedure());
}

TEST(Unit_Procedure, setEA)
{
    const gtirb::EA value{22678};

    auto node = std::make_unique<gtirb::Procedure>();
    EXPECT_NO_THROW(node->getEA());
    EXPECT_EQ(gtirb::EA{}, node->getEA());

    EXPECT_NO_THROW(node->setEA(value));
    EXPECT_EQ(value, node->getEA());
}

TEST(Unit_Procedure, getPLTEntries)
{
    auto procedure = std::make_unique<gtirb::Procedure>();
    EXPECT_NO_THROW(procedure->getPLTEntries());
}

TEST(Unit_Procedure, getPLTEntries_const)
{
    const auto procedure = std::make_unique<gtirb::Procedure>();
    EXPECT_NO_THROW(procedure->getPLTEntries());
}

TEST(Unit_Procedure, getPLTEntries_ref)
{
    const gtirb::EA entry{22678};

    auto procedure = std::make_unique<gtirb::Procedure>();

    // Scope so we can get the same reference again
    {
        auto &pltEntries = procedure->getPLTEntries();
        EXPECT_TRUE(pltEntries.empty());
        pltEntries.insert(entry);

        EXPECT_FALSE(pltEntries.empty());
        EXPECT_EQ(size_t{1}, pltEntries.size());
        EXPECT_EQ(std::begin(pltEntries), pltEntries.find(entry));
    }

    auto &pltEntries = procedure->getPLTEntries();
    EXPECT_FALSE(pltEntries.empty());
    EXPECT_EQ(size_t{1}, pltEntries.size());
    EXPECT_EQ(std::begin(pltEntries), pltEntries.find(entry));
}

TEST(Unit_Procedure, protobufRoundTrip)
{
    Procedure original;
    original.setEA(EA(1));
    original.getPLTEntries().insert(EA(2));

    gtirb::Procedure result;
    proto::Procedure message;
    original.toProtobuf(&message);
    result.fromProtobuf(message);

    EXPECT_EQ(result.getEA(), EA(1));
    EXPECT_EQ(result.getPLTEntries(), std::set<EA>({EA(2)}));
}
