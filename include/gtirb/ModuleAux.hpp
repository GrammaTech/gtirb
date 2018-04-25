#pragma once

#include <gtirb/ModuleSectionBase.hpp>

namespace gtirb
{
    ///
    /// \class ModuleAux
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API ModuleAux : public ModuleSectionBase
    {
    public:
    	ModuleAux();
        virtual ~ModuleAux() = default;

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int version)
        {
            ar& boost::serialization::base_object<ModuleSectionBase>(*this);
        }
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::ModuleAux);
