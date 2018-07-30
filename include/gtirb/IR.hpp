#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/Table.hpp>
#include <boost/filesystem.hpp>
#include <memory>
#include <vector>

namespace proto {
class IR;
}
namespace gtirb {
class Module;

///
/// \class IR
///
/// A complete IR consisting of Modules.
///
/// \dot
/// digraph example {
///     node [shape=record, fontname=Helvetica, fontsize=10];
///
///     ir [ label="gtirb::IR" URL="\ref IR"];
///     module [ label="gtirb::Module" URL="\ref Module"];
///     table [ label="gtirb::Table" URL="\ref Table"];
///     addrRanges [label="gtirb::AddrRanges" URL="\ref AddrRanges"]
///     dataSet [label="gtirb::DataSet" URL="\ref DataSet"]
///     imageByteMap [label="gtirb::ImageByteMap" URL="\ref ImageByteMap"]
///     sectionSet [label="gtirb::SectionSet" URL="\ref SectionSet"]
///     symbolSet [label="gtirb::SymbolSet" URL="\ref SymbolSet"]
///     symbolicOperandSet [label="gtirb::SymbolicExpressionSet" URL="\ref SymbolicExpressionSet"]
///     instruction [label="gtirb::Instruction" URL="\ref Instruction"]
///     cfg [label="gtirb::CFG" URL="\ref CFG"]
///     block [label="gtirb::Block" URL="\ref Block"]
///     data [label="gtirb::Data" URL="\ref Data"]
///     symbolicOperand [label="gtirb::SymbolicExpression" URL="\ref SymbolicExpression"]
///     section [label="gtirb::Section" URL="\ref Section"]
///     symbol [label="gtirb::Symbol" URL="\ref Symbol"]
///
///     ir -> module;
///     ir -> table;
///     module -> addrRanges;
///     module -> cfg;
///     module -> dataSet
///     module -> imageByteMap;
///     module -> sectionSet
///     module -> symbolSet;
///     module -> symbolicOperandSet
///     cfg -> block;
///     block -> instruction;
///     dataSet -> data;
///     sectionSet -> section;
///     symbolSet -> symbol;
///     symbolicOperandSet -> symbolicOperand;
/// }
/// \enddot
///
class GTIRB_EXPORT_API IR : public Node {
public:
  ///
  /// Default constructor.
  ///
  IR() = default;

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

  std::vector<Module>& getModules();
  const std::vector<Module>& getModules() const;

  ///
  /// Get all modules having the given Preferred EA
  ///
  /// \sa Module::getPreferredEA()
  ///
  std::vector<const Module*> getModulesWithPreferredEA(EA x) const;

  ///
  /// Get all modules continaing the given EA.
  ///
  /// The test is [lower bound inclusive, upper bound exclusive)
  ///
  /// \sa Module::getEAMinMax()
  ///
  std::vector<const Module*> getModulesContainingEA(EA x) const;

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
  /// \param x        The table itself.
  ///
  void addTable(std::string name, Table&& x);

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
  std::vector<Module> modules;
};
} // namespace gtirb
