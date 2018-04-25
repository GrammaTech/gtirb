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

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int version)
        {
            ar& boost::serialization::base_object<Node>(*this);
            ar& this->eas;
        }

    private:
        std::set<gtirb::EA> eas;
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::Region);
