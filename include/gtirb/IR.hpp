#pragma once

#include <boost/filesystem.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/Enums.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/Table.hpp>
#include <memory>
#include <vector>

namespace proto
{
    class IR;
}
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
        /// Copy constructor. Assigns a new UUID to the copy.
        ///
        explicit IR(const IR& other) = default;

        ///
        /// Move constructor
        ///
        IR(IR&&) = default;

        ///
        /// Move assignment
        ///
        IR& operator=(IR&&) = default;

        ///
        /// Trivial virtual destructor.
        ///
        ~IR() override = default;

        ///
        /// Gets a pointer to the module containing the program's "main".
        ///
        /// \return     nullptr if no main module has been created.
        ///
        Module& getMainModule();
        const Module& getMainModule() const;

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
        /// Serialize IR to an output stream.
        ///
        void save(std::ostream& out) const;

        ///
        /// Deserialize IR from an input stream.
        ///
        void load(std::istream& in);

        using MessageType = proto::IR;
        void toProtobuf(MessageType* message) const;
        void fromProtobuf(const MessageType& message);

        // ----------------------------------------------------------------------------------------
        // Table Properties

        ///
        /// Add a new table, transferring ownership.
        /// The table can be populated from anywhere.
        ///
        /// \param name     The name to assign to the table so it can be found later.
        /// \param x        An owning pointer to the table itself.
        /// \return         a reference to the added table.
        ///
        void addTable(std::string name, std::unique_ptr<Table>&& x);

        ///
        /// Get a table by name.
        ///
        /// \param  x   The name of the table to search for.
        /// \return     A non-owning pointer to the table if found, or nullptr.
        ///
        gtirb::Table* getTable(const std::string& x);

        ///
        /// Remove a table by name.
        ///
        /// This will invalidate any pointers that may have been held externally.
        ///
        /// \param  x   The name of the table to search for.
        /// \return     True on success.
        ///
        bool removeTable(const std::string& x);

        ///
        /// Get the total number of tables at this Node.
        ///
        /// \return     The total number of tables this node owns.
        ///
        size_t getTableSize() const;

        ///
        /// Test to see if the number of tables at this Node is zero.
        ///
        /// \return     True if this node does not own any tables.
        ///
        bool getTablesEmpty() const;

        ///
        /// Clear all locally owned tables.
        ///
        void clearTables();

    private:
        std::map<std::string, gtirb::Table> tables;
        std::vector<std::shared_ptr<Module>> modules;
        std::weak_ptr<gtirb::Module> mainModule{};
    };
} // namespace gtirb
