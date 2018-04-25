#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>

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
        RegionSet();

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
        Region* getRegion(gtirb::EA x) const;

        ///
        /// Get or create the symbol at the given EA.
        ///
        /// This is preferable to adding regions manually as it ensures no duplicate regions are
        /// created.
        ///
        /// \param x    The EA of the gtirb::Region to get (or create).
        /// \return     The Region at the given EA.
        ///
        Region* getOrCreateRegion(gtirb::EA x);

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/)
        {
            ar& boost::serialization::base_object<Node>(*this);
        }
    };
} // namespace gtirb

BOOST_CLASS_EXPORT_KEY(gtirb::RegionSet);
