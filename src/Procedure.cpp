#include <proto/Procedure.pb.h>
#include <gtirb/Instruction.hpp>
#include <gtirb/Procedure.hpp>
#include <gtirb/Serialization.hpp>

using namespace gtirb;

void Procedure::setEA(gtirb::EA x)
{
    this->ea = x;
}

gtirb::EA Procedure::getEA() const
{
    return this->ea;
}

std::set<gtirb::EA>& Procedure::getPLTEntries()
{
    return this->pltEntries;
}

const std::set<gtirb::EA>& Procedure::getPLTEntries() const
{
    return this->pltEntries;
}

Instruction* Procedure::createInstruction()
{
    this->instructions.emplace_back();
    return &this->instructions.back();
}

void Procedure::toProtobuf(MessageType* message) const
{
    nodeUUIDToBytes(this, *message->mutable_uuid());
    message->set_ea(this->ea);
    containerToProtobuf(this->pltEntries, message->mutable_plt_entries());
    containerToProtobuf(this->instructions, message->mutable_instructions());
}

void Procedure::fromProtobuf(const MessageType& message)
{
    setNodeUUIDFromBytes(this, message.uuid());
    this->ea = EA(message.ea());
    containerFromProtobuf(this->pltEntries, message.plt_entries());
    containerFromProtobuf(this->instructions, message.instructions());
}
