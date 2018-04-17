#pragma once

#include <boost/filesystem.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/Enums.hpp>
#include <gtirb/Node.hpp>

namespace gtirb
{
    ///
    /// \class IR
    /// \author John E. Farrier
    ///
    /// A complete IR consisting of Modules.
    ///
    class GTIRB_GTIRB_EXPORT_API IR final : public Node
    {
    public:
        IR();
        virtual ~IR() = default;

    private:
    };
}
