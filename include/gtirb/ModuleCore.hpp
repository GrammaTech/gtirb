#pragma once

#include <gtirb/ModuleSectionBase.hpp>

namespace gtirb
{
    ///
    /// \class ModuleCore
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API ModuleCore : public ModuleSectionBase
    {
    public:
    	ModuleCore();
        virtual ~ModuleCore() = default;

        template <class Archive>
        void serialize(Archive& ar, const unsigned int version)
        {
            ar& boost::serialization::base_object<ModuleSectionBase>(*this);
        }
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::ModuleCore);
