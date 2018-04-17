#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <set>

namespace gtirb
{
    ///
    /// \class FileMap
    /// \author John E. Farrier
    ///
    /// Contains the raw file data for the module (binary)
    ///
    class GTIRB_GTIRB_EXPORT_API FileMap : public Node
    {
    public:
        FileMap();
        virtual ~FileMap() = default;

    private:
    };
}
