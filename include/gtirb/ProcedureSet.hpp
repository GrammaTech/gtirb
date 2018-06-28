#pragma once

#include <gtirb/EA.hpp>
#include <map>

namespace gtirb
{
    class Procedure;
    using ProcedureSet = std::map<EA, Procedure>;
} // namespace gtirb
