#pragma once

#include <gtirb/CFGNodeInfo.hpp>

namespace gtirb
{
    ///
    /// \class CFGNodeInfoEntry
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API CFGNodeInfoActualIn final : public CFGNodeInfo
    {
    public:
        ///
        /// Defaulted trivial destructor.
        ///
        virtual ~CFGNodeInfoActualIn() = default;

    private:
    };
}
