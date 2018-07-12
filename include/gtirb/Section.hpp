#pragma once

#include <cstdint>
#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>

namespace proto
{
    class Section;
}
namespace gtirb
{
    ///
    /// \class Section
    /// \author Nathan Weston
    struct GTIRB_GTIRB_EXPORT_API Section : public Node
    {
    public:
        Section() = default;
        Section(std::string n, uint64_t size, EA address);

        std::string name;
        uint64_t size{0};
        EA startingAddress{0};

        /// The exclusive limit of the section. I.e. the smallest EA which is
        /// past the end.
        EA addressLimit() const;

        /// Is this address within the section?
        bool contains(EA ea) const;

        bool operator==(const Section& other) const;
        bool operator!=(const Section& other) const;

        using MessageType = proto::Section;
        void toProtobuf(MessageType* message) const;
        void fromProtobuf(const MessageType& message);
    };
}
