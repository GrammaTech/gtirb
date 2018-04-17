#pragma once

#include <gtirb/ModuleSectionBase.hpp>

namespace gtirb
{
    ///
    /// \class ModuleAux
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API ModuleAux final : public ModuleSectionBase
    {
    public:
    	ModuleAux();
        virtual ~ModuleAux() = default;
    };
}
