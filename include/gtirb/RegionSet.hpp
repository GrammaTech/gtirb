#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/Region.hpp>
#include <vector>

namespace gtirb
{
    class Region;

    ///
    /// \class RegionSet
    /// \author John E. Farrier
    ///
    /// Storage for all gtirb::Region objects for a single gtirb::Module.
    /// This has gtirb::Region children.
    ///
    class GTIRB_GTIRB_EXPORT_API RegionSet : public Node
    {
    public:
        ///
        /// Default constructor.
        ///
        RegionSet() = default;

        ///
        /// Defaulted trivial destructor.
        ///
        ~RegionSet() override = default;

        ///
        /// Get the symbol at the given EA.
        ///
        /// \param x    The EA of the gtirb::Region to get.
        /// \return     The Region at the given EA or nullptr.
        ///
        const Region* getRegion(gtirb::EA x) const;

        ///
        /// Create a region with the given EA.
        ///
        /// This is preferable to adding regions manually as it ensures no duplicate regions are
        /// created.
        ///
        /// \param x    The EA of the gtirb::Region to get (or create).
        /// \return     A reference to the new region.
        ///
        Region& createRegion(gtirb::EA x);

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/)
        {
            ar& boost::serialization::base_object<Node>(*this);
        }

    private:
        std::vector<Region> contents;
    };
} // namespace gtirb

BOOST_CLASS_EXPORT_KEY(gtirb::RegionSet);
