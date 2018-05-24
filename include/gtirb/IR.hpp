#pragma once

#include <boost/filesystem.hpp>
#include <boost/serialization/weak_ptr.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/Enums.hpp>
#include <gtirb/Node.hpp>
#include <memory>
#include <vector>

namespace gtirb
{
    class Module;

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
    ///     regionSet [label="gtirb::RegionSet" URL="\ref RegionSet"]
    ///     symbolSet [label="gtirb::SymbolSet" URL="\ref SymbolSet"]
    ///     imageByteMap [label="gtirb::ImageByteMap" URL="\ref ImageByteMap"]
    ///     symbol [label="gtirb::Symbol" URL="\ref Symbol"]
    ///     procedure [label="gtirb::Procedure" URL="\ref Procedure"]
    ///     procedureSet [label="gtirb::ProcedureSet" URL="\ref ProcedureSet"]
    ///     instruction [label="gtirb::Instruction" URL="\ref Instruction"]
    ///     cfg [label="gtirb::CFG" URL="\ref CFG"]
    ///     cfgSet [label="gtirb::CFGSet" URL="\ref CFGSet"]
    ///     cfgNode [label="gtirb::CFGNode" URL="\ref CFGNode"]
    ///     cfgNodeInfoActualIn [label="gtirb::CFGNodeInfoActualIn" URL="\ref CFGNodeInfoActualIn"]
    ///     cfgNodeInfoDeclares [label="gtirb::CFGNodeInfoDeclares" URL="\ref CFGNodeInfoDeclares"]
    ///     cfgNodeInfoEntry [label="gtirb::CFGNodeInfoEntry" URL="\ref CFGNodeInfoEntry"]
    ///     cfgNodeInfoFormalIn [label="gtirb::CFGNodeInfoFormalIn" URL="\ref CFGNodeInfoFormalIn"]
    ///     cfgNodeInfoCall [label="gtirb::CFGNodeInfoCall" URL="\ref CFGNodeInfoCall"]
    ///
    ///     ir -> module;
    ///     module -> moduleSummary;
    ///     module -> moduleCore;
    ///     module -> moduleAux;
    ///     module -> addrRanges;
    ///     module -> regionSet;
    ///     regionSet -> region;
    ///     module -> symbolSet;
    ///     symbolSet -> symbol;
    ///     module -> imageByteMap;
    ///     module -> procedureSet;
    ///     procedureSet -> procedure;
    ///     procedure -> instruction;
    ///     module -> cfgSet;
    ///     cfgSet -> cfg;
    ///     cfg -> cfgNode;
    ///     cfgNode -> cfgNodeInfoActualIn;
    ///     cfgNode -> cfgNodeInfoDeclares;
    ///     cfgNode -> cfgNodeInfoEntry;
    ///     cfgNode -> cfgNodeInfoFormalIn;
    ///     cfgNode -> cfgNodeInfoCall;
    /// }
    /// \enddot
    ///
    class GTIRB_GTIRB_EXPORT_API IR : public Node
    {
    public:
        ///
        /// Default constructor.
        ///
        IR();

        ///
        /// Trivial virtual destructor.
        ///
        ~IR() override = default;

        ///
        /// Gets a pointer to the module containing the program's "main".
        ///
        /// \return     nullptr if no main module has been created.
        ///
        gtirb::Module* getMainModule() const;

        ///
        /// Get or create a module containin the program's "main".
        ///
        gtirb::Module* createMainModule();

        ///
        /// Get all modules having the given Preferred EA
        ///
        /// \sa Module::getPreferredEA()
        ///
        std::vector<gtirb::Module*> getModulesWithPreferredEA(EA x) const;

        ///
        /// Get all modules continaing the given EA.
        ///
        /// The test is [lower bound inclusive, upper bound exclusive)
        ///
        /// \sa Module::getEAMinMax()
        ///
        std::vector<gtirb::Module*> getModulesContainingEA(EA x) const;

        ///
        /// Add a new module to the IR.
        ///
        void addModule(std::unique_ptr<gtirb::Module>&& x);

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/)
        {
            ar& boost::serialization::base_object<Node>(*this);
            ar & this->mainModule;
        }

    private:
        std::vector<std::shared_ptr<Module>> modules;
        std::weak_ptr<gtirb::Module> mainModule{};
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::IR);
