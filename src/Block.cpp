#include <proto/Block.pb.h>
#include <gtirb/Block.hpp>
#include <gtirb/Instruction.hpp>
#include <gtirb/Serialization.hpp>

using namespace gtirb;

Block::Block(EA startingAddress_, EA endingAddress_)
    : Node(), startingAddress(startingAddress_), endingAddress(endingAddress_)
{
}

Block::Block(EA startingAddress_, EA endingAddress_, std::vector<Instruction>&& instructions_)
    : Node(),
      startingAddress(startingAddress_),
      endingAddress(endingAddress_),
      instructions(std::move(instructions_))
{
}

EA Block::getStartingAddress() const
{
    return this->startingAddress;
}
EA Block::getEndingAddress() const
{
    return this->endingAddress;
}

std::vector<Instruction>& Block::getInstructions()
{
    return this->instructions;
}

const std::vector<Instruction>& Block::getInstructions() const
{
    return this->instructions;
}

void Block::toProtobuf(MessageType* message) const
{
    nodeUUIDToBytes(this, *message->mutable_uuid());
    message->set_starting_address(this->startingAddress);
    message->set_ending_address(this->endingAddress);
    containerToProtobuf(this->instructions, message->mutable_instructions());
}

void Block::fromProtobuf(const MessageType& message)
{
    setNodeUUIDFromBytes(this, message.uuid());
    this->startingAddress = EA(message.starting_address());
    this->endingAddress = EA(message.ending_address());
    containerFromProtobuf(this->instructions, message.instructions());
}
