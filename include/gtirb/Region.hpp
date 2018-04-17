#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <set>

namespace gtirb
{
    ///
    /// \class Region
    /// \author John E. Farrier
    ///
    /// A base class for Module Regions.
    ///
    class GTIRB_GTIRB_EXPORT_API Region : public Node
    {
    public:
        Region();
        virtual ~Region() = default;

        void addEA(gtirb::EA x);
        std::set<gtirb::EA> getEAs() const;

    private:
        std::set<gtirb::EA> eas;
    };
}
