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
    };
}
