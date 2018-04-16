#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>

namespace gtirb
{
    ///
    /// \class AddrRanges
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API AddrRanges : public Node
    {
    public:
        ///
        /// Default Constructor.
        ///
        AddrRanges();

        ///
        /// Defaulted trivial destructor.
        ///
        virtual ~AddrRanges() = default;

        ///
        /// Add a pair of EA's to the internal vector.
        /// This will perform a check on the EA's to ensure that the first is less than or equal to the second.
        ///
        /// Throws a gtirb::RuntimeError if the EA's are not properly ordered.
        ///
        /// \param  x   A pair of EA's to add to the vector.
        ///
        void addRange(std::pair<gtirb::EA, gtirb::EA> x);

        ///
        /// Get all of the ranges that have been added.
        ///
        std::vector<std::pair<gtirb::EA, gtirb::EA>>& getRangeVector();
        
        ///
        /// Get all of the ranges that have been added.
        ///
        const std::vector<std::pair<gtirb::EA, gtirb::EA>>& getRangeVector() const;

        ///
        /// Given all of the ranges, how many bytes exist between all pairs?
        ///
        /// \return     The number of bytes in all of the ranges in the vector.
        ///
        size_t getBytesCoveredByRanges() const;

    private:
        std::vector<std::pair<gtirb::EA, gtirb::EA>> rangeVector;
    };
}
