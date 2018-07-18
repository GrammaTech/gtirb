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

TEST(Unit_Instruction, protobufRoundTrip)
{
    Instruction original(EA(1));

    gtirb::Instruction result;
    proto::Instruction message;
    original.toProtobuf(&message);
    original.setUUID(); // Avoid UUID conflict
    result.fromProtobuf(message);

    EXPECT_EQ(result.getEA(), EA(1));
}
