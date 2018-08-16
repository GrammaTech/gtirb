#ifndef GTIRB_IR_H
#define GTIRB_IR_H

#include <gtirb/Addr.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/Table.hpp>
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
///     dataSet [label="gtirb::DataSet" URL="\ref DataSet"]
///     imageByteMap [label="gtirb::ImageByteMap" URL="\ref ImageByteMap"]
///     sectionSet [label="gtirb::SectionSet" URL="\ref SectionSet"]
///     symbolSet [label="gtirb::SymbolSet" URL="\ref SymbolSet"]
///     symbolicOperandSet [label="gtirb::SymbolicExpressionSet" URL="\ref
///     SymbolicExpressionSet"] cfg [label="gtirb::CFG" URL="\ref CFG"] block
///     [label="gtirb::Block" URL="\ref Block"] data [label="gtirb::DataObject"
///     URL="\ref DataObject"] symbolicOperand
///     [label="gtirb::SymbolicExpression" URL="\ref SymbolicExpression"]
///     section [label="gtirb::Section" URL="\ref Section"]
///     symbol [label="gtirb::Symbol" URL="\ref Symbol"]
///
///     ir -> module;
///     ir -> table;
///     module -> cfg;
///     module -> dataSet
///     module -> imageByteMap;
///     module -> sectionSet
///     module -> symbolSet;
///     module -> symbolicOperandSet
///     cfg -> block;
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
  IR();

  ///
  /// IR may not be copied due to Modules.
  ///
  IR(const IR&) = delete;

  ///
  /// Move constructor
  ///
  IR(IR&&);

  ///
  /// Move assignment
  ///
  IR& operator=(IR&&);

  ///
  /// Trivial virtual destructor.
  ///
  ~IR() override;

  std::vector<Module>& getModules();
  const std::vector<Module>& getModules() const;

  ///
  /// Get all modules having the given Preferred address
  ///
  /// \sa Module::getPreferredAddr()
  ///
  std::vector<const Module*> getModulesWithPreferredAddr(Addr X) const;

  ///
  /// Get all modules continaing the given address.
  ///
  /// The test is [lower bound inclusive, upper bound exclusive)
  ///
  /// \sa Module::getAddrMinMax()
  ///
  std::vector<const Module*> getModulesContainingAddr(Addr X) const;

  ///
  /// Serialize IR to an output stream.
  ///
  void save(std::ostream& Out) const;

  ///
  /// Deserialize IR from an input stream.
  ///
  void load(std::istream& In);

  using MessageType = proto::IR;
  void toProtobuf(MessageType* Message) const;
  void fromProtobuf(const MessageType& Message);

  // ----------------------------------------------------------------------------------------
  // Table Properties

  ///
  /// Add a new table, transferring ownership.
  /// The table can be populated from anywhere.
  ///
  /// \param name     The name to assign to the table so it can be found later.
  /// \param X        The table itself.
  ///
  void addTable(std::string Name, Table&& X);

  ///
  /// Get a table by name.
  ///
  /// \param  X   The name of the table to search for.
  /// \return     A non-owning pointer to the table if found, or nullptr.
  ///
  const gtirb::Table* getTable(const std::string& X) const;
  gtirb::Table* getTable(const std::string& X);

  ///
  /// Remove a table by name.
  ///
  /// This will invalidate any pointers that may have been held externally.
  ///
  /// \param  X   The name of the table to search for.
  /// \return     True on success.
  ///
  bool removeTable(const std::string& X);

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
  std::map<std::string, gtirb::Table> Tables;
  std::vector<Module> Modules;
};
} // namespace gtirb

#endif // GTIRB_IR_H
