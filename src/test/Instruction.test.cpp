#include <gtest/gtest.h>
#include <proto/Instruction.pb.h>
#include <gtirb/Block.hpp>
#include <gtirb/Instruction.hpp>
#include <memory>

using namespace gtirb;

TEST(Unit_Instruction, ctor_0)
{
    EXPECT_NO_THROW(gtirb::Instruction());
}

TEST(Unit_Instruction, setEA)
{
    const gtirb::EA ea{22678};

    gtirb::Instruction node;
    EXPECT_NO_THROW(node.setEA(ea));
    EXPECT_EQ(ea, node.getEA());
}

TEST(Unit_Instruction, getEA)
{
    const gtirb::EA ea{};

    gtirb::Instruction node;
    EXPECT_EQ(ea, node.getEA());
}

TEST(Unit_Instruction, setIsFallthrough)
{
    gtirb::Instruction node;

    EXPECT_NO_THROW(node.setIsFallthrough(true));
    EXPECT_EQ(true, node.getIsFallthrough());

    EXPECT_NO_THROW(node.setIsFallthrough(false));
    EXPECT_EQ(false, node.getIsFallthrough());
}

TEST(Unit_Instruction, getIsFallthrough)
{
    gtirb::Instruction node;
    EXPECT_EQ(false, node.getIsFallthrough());
}

TEST(Unit_Instruction, setIsPEI)
{
    gtirb::Instruction node;

    EXPECT_NO_THROW(node.setIsPEI(true));
    EXPECT_EQ(true, node.getIsPEI());

    EXPECT_NO_THROW(node.setIsPEI(false));
    EXPECT_EQ(false, node.getIsPEI());
}

TEST(Unit_Instruction, getIsPEI)
{
    gtirb::Instruction node;
    EXPECT_EQ(false, node.getIsPEI());
}

TEST(Unit_Instruction, setNumberOfUses)
{
    const int64_t numberOfUses{22678};

    gtirb::Instruction node;
    EXPECT_NO_THROW(node.setNumberOfUses(numberOfUses));
    EXPECT_EQ(numberOfUses, node.getNumberOfUses());
}

TEST(Unit_Instruction, getNumberOfUses)
{
    gtirb::Instruction node;
    EXPECT_EQ(int64_t{0}, node.getNumberOfUses());
}

TEST(Unit_Instruction, protobufRoundTrip)
{
    Instruction original(EA(1));
    original.setNumberOfUses(2);
    original.setIsFallthrough(true);
    original.setIsPEI(true);

    gtirb::Instruction result;
    proto::Instruction message;
    original.toProtobuf(&message);
    result.fromProtobuf(message);

    EXPECT_EQ(result.getEA(), EA(1));
    EXPECT_EQ(result.getNumberOfUses(), 2);
    EXPECT_EQ(result.getIsFallthrough(), true);
    EXPECT_EQ(result.getIsPEI(), true);
}
