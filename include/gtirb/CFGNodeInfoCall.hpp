#pragma once

#include <gtirb/CFGNodeInfo.hpp>
#include <gtirb/EA.hpp>

namespace gtirb
{
    ///
    /// \class CFGNodeInfoEntry
    /// \author John E. Farrier
    ///
    /// \todo Remove.  This may will eventually become something we
    /// store in an external table.
    ///
    class GTIRB_GTIRB_EXPORT_API CFGNodeInfoCall : public CFGNodeInfo
    {
    public:
        ///
        /// Defaulted trivial destructor.
        ///
        ~CFGNodeInfoCall() override = default;

        void setKey(int64_t x);
        int64_t getKey() const;

        void setReturnSpAdjust(int64_t x);
        int64_t getReturnSpAdjust() const;

        void setImportTableEntryEA(gtirb::EA x);
        gtirb::EA getImportTableEntryEA() const;

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/)
        {
            ar& boost::serialization::base_object<CFGNodeInfo>(*this);
            ar & this->importTableEntryEa;
            ar & this->key;
            ar & this->returnSpAdjust;
        }

    private:
        gtirb::EA importTableEntryEa{};
        int64_t key{0};
        int64_t returnSpAdjust{0};
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::CFGNodeInfoCall);
