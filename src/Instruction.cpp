#include <proto/Instruction.pb.h>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Instruction.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Serialization.hpp>

using namespace gtirb;

Instruction::Instruction(EA ea_, uint64_t size_) : Node(), ea(ea_), size(size_)
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

uint64_t Instruction::getSize() const
{
    return this->size;
}

std::vector<uint8_t> Instruction::getBytes(const Module& module) const
{
    return module.getImageByteMap().getData(this->getEA(), this->getSize());
}

void Instruction::toProtobuf(MessageType* message) const
{
    nodeUUIDToBytes(this, *message->mutable_uuid());
    message->set_ea(this->ea);
    message->set_size(this->size);
}

void Instruction::fromProtobuf(const MessageType& message)
{
    setNodeUUIDFromBytes(this, message.uuid());
    this->ea = EA(message.ea());
    this->size = message.size();
}
