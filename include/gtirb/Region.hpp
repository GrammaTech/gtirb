#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <set>

namespace proto
{
    class Region;
}
namespace gtirb
{
    ///
    /// \class Region
    /// \author John E. Farrier
    ///
    /// A base class for Module Regions.
    ///
    /// \todo   Several different gtirb::Region subclasses may be created to encompass Global,
    /// Module, Abstract, Heap, Stack, and Extern regions.  What goes into these classes is
    /// undefined in my notes.
    ///
    class GTIRB_GTIRB_EXPORT_API Region : public Node
    {
    public:
        Region() = default;

        ///
        /// Copy constructor. Assigns a new UUID to the copy.
        ///
        explicit Region(const Region& other) = default;

        ///
        /// Move constructor
        ///
        Region(Region&&) = default;

        ///
        /// Move assignment
        ///
        Region& operator=(Region&&) = default;

        ~Region() override = default;

        std::set<gtirb::EA>& getEAs();
        const std::set<gtirb::EA>& getEAs() const;

        using MessageType = proto::Region;
        void toProtobuf(MessageType* message) const;
        void fromProtobuf(const MessageType& message);

    private:
        std::set<gtirb::EA> eas;
    };
}
