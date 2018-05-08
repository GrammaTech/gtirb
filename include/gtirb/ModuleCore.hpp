#pragma once

#include <gtirb/ModuleSectionBase.hpp>

namespace gtirb
{
    ///
    /// \class ModuleCore
    /// \author John E. Farrier
    ///
    /// \todo   gtirb::ModuleCore can likely be combined into gtirb::Module.
    ///
    class GTIRB_GTIRB_EXPORT_API ModuleCore : public ModuleSectionBase
    {
    public:
        ModuleCore();
        ~ModuleCore() override = default;

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/)
        {
            ar& boost::serialization::base_object<ModuleSectionBase>(*this);
        }
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::ModuleCore);
