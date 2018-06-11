#pragma once

#include <gtirb/CFGNodeInfo.hpp>

namespace gtirb
{
    ///
    /// \class CFGNodeInfoEntry
    /// \author John E. Farrier
    ///
    /// \todo Remove.  This may will eventually become something we
    /// store in an external table.
    ///
    class GTIRB_GTIRB_EXPORT_API CFGNodeInfoDeclares : public CFGNodeInfo
    {
    public:
        ///
        /// Defaulted trivial destructor.
        ///
        ~CFGNodeInfoDeclares() override = default;

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/)
        {
            ar& boost::serialization::base_object<CFGNodeInfo>(*this);
        }

    private:
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::CFGNodeInfoDeclares);
