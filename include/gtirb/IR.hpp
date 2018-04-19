#pragma once

#include <boost/filesystem.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/Enums.hpp>
#include <gtirb/Node.hpp>

namespace gtirb
{
    ///
    /// \class IR
    /// \author John E. Farrier
    ///
    /// A complete IR consisting of Modules.
    ///
    /// \dot
    /// digraph example {
    ///     node [shape=record, fontname=Helvetica, fontsize=10];
    ///
    ///     ir [ label="gtirb::IR" URL="\ref IR"];
    ///     module [ label="gtirb::Module" URL="\ref Module"];
    ///     moduleSummary [label="gtirb::ModuleSummary" URL="\ref ModuleSummary"]
    ///     moduleCore [label="gtirb::ModuleCore" URL="\ref ModuleCore"]
    ///     moduleAux [label="gtirb::ModuleAux" URL="\ref ModuleAux"]
    ///     addrRanges [label="gtirb::AddrRanges" URL="\ref AddrRanges"]
    ///     region [label="gtirb::Region" URL="\ref Region"]
    ///     symbolSet [label="gtirb::SymbolSet" URL="\ref SymbolSet"]
    ///     imageByteMap [label="gtirb::ImageByteMap" URL="\ref ImageByteMap"]
    ///     symbol [label="gtirb::Symbol" URL="\ref Symbol"]
    ///
    ///     ir -> module;
    ///     module -> moduleSummary;
    ///     module -> moduleCore;
    ///     module -> moduleAux;
    ///     module -> addrRanges;
    ///     module -> region;
    ///     module -> symbolSet;
    ///     module -> imageByteMap;
    ///     symbolSet -> symbol;
    /// }
    /// \enddot
    ///
    class GTIRB_GTIRB_EXPORT_API IR final : public Node
    {
    public:
        IR();
        virtual ~IR() = default;

    private:
    };
}
