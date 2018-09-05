#ifndef GTIRB_NODE_H
#define GTIRB_NODE_H

#include <gtirb/Casting.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/Export.hpp>
#include <gsl/gsl>
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
  static Node* getByUUID(Context& C, const UUID& Uuid) {
    return C.findNode(Uuid);
  }

  static Node* Create(Context& C) { return new (C) Node(C, Kind::Node); }

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
  Node(Context& C, Kind K);

private:
  Kind K;
  UUID Uuid;
  // The Context object can never be null as it can only be passed to the Node
  // constructor by reference. However, we don't want to store a reference to
  // the Context object because we want to keep the Node class copyable and
  // Context needs to own a move-only allocator.
  gsl::not_null<Context*> Ctx;

  friend void details::ClearUUIDs(Node*);
  void setUUID();
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
