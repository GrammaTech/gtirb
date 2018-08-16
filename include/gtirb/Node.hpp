#ifndef GTIRB_NODE_H
#define GTIRB_NODE_H

#include <gtirb/Context.hpp>
#include <gtirb/Export.hpp>
#include <boost/uuid/uuid.hpp>
#include <map>
#include <string>

namespace gtirb {
using UUID = boost::uuids::uuid;
class Node;

namespace details {
void GTIRB_EXPORT_API ClearUUIDs(Node*);
}

/// \brief DOCFIXME
///
using UUID = boost::uuids::uuid;

/// \class Node
///
/// \brief DOCFIXME
///
class GTIRB_EXPORT_API Node {
public:
  enum class Kind {
    Node,
    Block,
    DataObject,
    ImageByteMap,
    IR,
    Module,
    Section,
    Symbol,
  };

  ///
  /// Retrieve a node by its UUID.
  ///
  /// \return The Node with the given UUID, or nullptr if none exists.
  static Node* getByUUID(UUID Uuid);

  static Node* Create(Context& C) { return new (C) Node(Kind::Node); }

  /// \brief This will serve as a base class for other nodes.
  ///
  ~Node() noexcept;

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

  Kind getKind() const { return K; }
  static bool classof(const Node* N) { return N->getKind() == Kind::Node; }

protected:
  Node(Kind K);

private:
  Kind K;
  UUID Uuid;

  friend void details::ClearUUIDs(Node*);
  void setUUID();

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
