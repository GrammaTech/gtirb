#ifndef GTIRB_NODE_H
#define GTIRB_NODE_H

#include <gtirb/Export.hpp>
#include <boost/uuid/uuid.hpp>
#include <functional>
#include <gsl/gsl>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace gtirb {
using UUID = boost::uuids::uuid;

///
/// \class Node
///
/// \brief DOCFIXME
///
class GTIRB_EXPORT_API Node {
public:

  /// \brief Retrieve a node by its UUID.
  ///
  /// \return The Node with the given UUID, or nullptr if none exists.
  static Node* getByUUID(UUID Uuid);


  /// \brief Default constructor.
  /// Automatically assigns the Node a UUID.
  ///
  Node();

  ///
  /// \brief Copy constructor.
  /// The copy is assigned a new UUID.
  ///
  explicit Node(const Node&);


  /// \brief Nodes are not assignable due to UUIDs.
  ///
  Node& operator=(const Node&) = delete;


  /// \brief Move constructor.
  /// Clears the UUID of the moved node and updates the UUID to node
  /// mapping.
  ///
  Node(Node&& other) noexcept;


  /// \brief Move assignment.
  /// Clears the UUID of the moved node and updates the UUID to node
  /// mapping.
  ///
  Node& operator=(Node&& other) noexcept;


  /// \brief This will serve as a base class for other nodes.
  ///
  virtual ~Node() noexcept;


  /// DOCFIXME[check all]
  /// \brief Set the Universally Unique ID (UUID) for \c this to a
  /// newly-generated value.
  ///
  /// \return void
  ///
  /// Each Node is automatically assigned a UUID on construction; this
  /// method allows a different UUID to be assigned later.
  /// DOCFIXME[what is the use case?]
  ///
  void setUUID();


  /// DOCFIXME[check all]
  /// \brief Set the Universally Unique ID (UUID) for \c this to the
  /// specified value.
  ///
  /// \return void
  ///
  /// Each Node is automatically assigned a UUID on construction; this
  /// method allows a different UUID to be assigned later.
  /// DOCFIXME[what is the use case? how is uniqueness enforced on X?] 
  ///
  void setUUID(UUID X);


  /// \brief Get the Universally Unique ID (UUID) for \c this.
  ///
  /// \return The UUID.
  ///
  UUID getUUID() const { return Uuid; }

private:
  UUID Uuid;

  static std::map<UUID, Node*> UuidMap;
};


/// DOCFIXME[check all]
/// \brief Return the text representation of a UUID.
///
/// \param Uuid The UUID of interest.
///
/// \return The text representation of \p Uuid.
///  
std::string uuidToString(const UUID& Uuid);

/// DOCFIXME[check all]
/// \brief Create a UUID from a text representation.
///
/// \param Text The text representation. DOCFIXME[any constraints?]
///
/// \return A new UUID corresponding to \p Text.
///
/// DOCFIXME[what if I call twice with the same Text?]
///  
UUID uuidFromString(const std::string& Text);

} // namespace gtirb

#endif // GTIRB_NODE_H
