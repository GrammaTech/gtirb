#pragma once

#include <gtirb/CFGNodeInfo.hpp>

namespace gtirb
{
    ///
    /// \class CFGNodeInfoEntry
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API CFGNodeInfoFormalIn final : public CFGNodeInfo
    {
    public:
        ///
        /// Defaulted trivial destructor.
        ///
        virtual ~CFGNodeInfoFormalIn() = default;

    private:
    };
}
