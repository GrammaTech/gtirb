#include <proto/Instruction.pb.h>
#include <gtirb/Block.hpp>
#include <gtirb/Instruction.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/RuntimeError.hpp>
#include <gtirb/Serialization.hpp>

using namespace gtirb;
class Block;

Instruction::Instruction(EA ea) : Node(), ea(ea)
{
}

void Instruction::setEA(gtirb::EA x)
{
    this->ea = x;
}

gtirb::EA Instruction::getEA() const
{
    return this->ea;
}

void Instruction::setIsFallthrough(bool x)
{
    this->isFallthrough = x;
}

bool Instruction::getIsFallthrough() const
{
    return this->isFallthrough;
}

void Instruction::setIsPEI(bool x)
{
    this->isPEI = x;
}

bool Instruction::getIsPEI() const
{
    return this->isPEI;
}

void Instruction::setNumberOfUses(int64_t x)
{
    this->numberOfUses = x;
}

int64_t Instruction::getNumberOfUses() const
{
    return this->numberOfUses;
}

void Instruction::toProtobuf(MessageType* message) const
{
    nodeUUIDToBytes(this, *message->mutable_uuid());
    message->set_ea(this->ea);
    message->set_number_of_uses(this->numberOfUses);
    message->set_is_fallthrough(this->isFallthrough);
    message->set_is_pei(this->isPEI);
}

void Instruction::fromProtobuf(const MessageType& message)
{
    setNodeUUIDFromBytes(this, message.uuid());
    this->ea = EA(message.ea());
    this->numberOfUses = message.number_of_uses();
    this->isFallthrough = message.is_fallthrough();
    this->isPEI = message.is_pei();
}
