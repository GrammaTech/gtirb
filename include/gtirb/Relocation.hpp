#pragma once

#include <proto/Relocation.pb.h>
#include <gtirb/EA.hpp>
#include <string>

namespace gtirb
{
    ///
    /// \class Relocation
    ///
    /// \todo Figure out how best to represent this.
    struct Relocation
    {
        EA ea{0};
        std::string type;
        std::string name;
        uint64_t offset{0};

        using MessageType = proto::Relocation;
        void toProtobuf(MessageType* message) const
        {
            message->set_ea(this->ea);
            message->set_type(this->type);
            message->set_name(this->name);
            message->set_offset(this->offset);
        }

        void fromProtobuf(const MessageType& message)
        {
            this->ea = EA(message.ea());
            this->type = message.type();
            this->name = message.name();
            this->offset = message.offset();
        }
    };
}
