#ifndef GTIRB_IR_H
#define GTIRB_IR_H

#include <gtirb/Addr.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/Table.hpp>
#include <map>
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
/// \brief A complete internal representation consisting of Modules
/// (\ref Module).
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

  /// \brief Default constructor.
  ///
  IR();


  /// \brief IR may not be copied due to Modules. DOCFIXME[this is a bit mysterious]
  ///
  IR(const IR&) = delete;


  /// \brief Move constructor.
  ///
  IR(IR&&);


  /// \brief Move assignment.
  ///
  IR& operator=(IR&&);

  ///
  /// \brief Trivial virtual destructor.
  ///
  ~IR() override;


  /// DOCFIXME[check all]
  ///
  /// \brief Get all modules (\ref Module).
  ///
  /// \return All modules associated with \c this, as a \c
  /// std::vector<Module>.
  ///
  std::vector<Module>& getModules() { return Modules; }


  /// DOCFIXME[check all]
  ///
  /// \brief Get all modules (\ref Module), as a const reference.
  ///
  /// \return All modules associated with \c this, as a 
  /// <tt>const std::vector<Module></tt>.
  ///
  const std::vector<Module>& getModules() const { return Modules; }


  /// \brief Get all modules with the specified Preferred Effective Address.
  ///
  /// \param X The address of interest.
  ///
  /// \return A <tt>std::vector<const Module*></tt> containing all
  /// modules whose Preferred Effective Address is \p X.
  ///
  /// \sa Module::getPreferredAddr()
  ///
  std::vector<const Module*> getModulesWithPreferredAddr(Addr X) const;


  /// \brief Get all modules containing the specified address.
  ///
  /// \param X The address of interest.
  ///
  /// \return A <tt>std::vector<const Module*></tt> containing all
  /// modules with (minimum,maximum) effective addresses such that
  /// minimum <= X < maximum.
  ///
  /// \sa Module::getAddrMinMax()
  ///
  std::vector<const Module*> getModulesContainingAddr(Addr X) const;


  /// \brief Serialize to an output stream.
  ///
  /// \param Out The output stream.
  ///
  /// \return void
  ///
  void save(std::ostream& Out) const;


  /// \brief Deserialize from an input stream.
  ///
  /// \param In  The input stream.
  ///
  /// \return void
  ///
  void load(std::istream& In);


  /// \brief DOCFIXME
  using MessageType = proto::IR;

  ///
  /// \brief DOCFIXME
  ///
  /// \param Message DOCFIXME
  ///
  /// \return void
  ///
  void toProtobuf(MessageType* Message) const;

  ///
  /// \brief DOCFIXME
  ///
  /// \param Message DOCFIXME
  ///
  /// \return void
  ///
  void fromProtobuf(const MessageType& Message);

  // ----------------------------------------------------------------------
  // Table Properties


  /// \brief Add a new table, transferring ownership.
  /// The table can be populated from anywhere.
  ///
  /// \param Name     The name to assign to the table so it can be found later.
  /// \param X        The table itself.
  /// \return void
  ///
  void addTable(std::string Name, Table&& X);


  /// \brief Get a table by name.
  ///
  /// \param  X   The name of the table to search for.
  ///
  /// \return     A non-owning pointer to the table if found,
  ///             \c nullptr otherwise.
  ///
  const gtirb::Table* getTable(const std::string& X) const;


  /// \brief Get a table by name.
  /// DOCFIXME[distinction  from previous]
  ///
  /// \param  X   The name of the table to search for.
  ///
  /// \return     A non-owning pointer to the table if found,
  ///             \c nullptr otherwise.
  ///
  gtirb::Table* getTable(const std::string& X);


  /// \brief Remove a table by name.
  ///
  /// This will invalidate any pointers that may have been held externally.
  ///
  /// \param  X   The name of the table to search for.
  /// \return     \c true on success, \c false otherwise.
  ///
  bool removeTable(const std::string& X);


  /// \brief Get the total number of tables at this Node.
  /// DOCFIXME[what Node?]
  ///
  /// \return     The total number of tables this node owns.
  ///
  size_t getTableSize() const { return Tables.size(); }


  /// \brief Check: Is the number of tables at this Node zero?
  /// DOCFIXME[what Node?]
  ///
  /// \return \c true if this node does not own any tables, \c false
  /// if it owns one or more tables.
  ///
  bool getTablesEmpty() const { return Tables.empty(); }


  /// \brief Clear all locally owned tables.
  ///
  /// \return void
  ///
  void clearTables() { Tables.clear(); }

private:
  std::map<std::string, gtirb::Table> Tables;
  std::vector<Module> Modules;
};
} // namespace gtirb

#endif // GTIRB_IR_H
