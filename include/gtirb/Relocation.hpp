#pragma once

#include <boost/serialization/export.hpp>
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

        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/)
        {
            ar & this->ea;
            ar & this->type;
            ar & this->name;
            ar & this->offset;
        }
    };
}
