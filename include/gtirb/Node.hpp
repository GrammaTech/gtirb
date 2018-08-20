#pragma once

#include <gtirb/Context.hpp>
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

  Node(const Node &) = delete;
  void operator=(const Node &) = delete;

  ///
  /// Retrieve a node by its UUID.
  ///
  /// \return node with the given UUID, or nullptr if none exists.
  static Node* getByUUID(UUID Uuid);

  static Node *Create(Context &C) { return new (C) Node(Kind::Node); }

  ///
  /// This will serve as a base class for other nodes.
  ///
  virtual ~Node() noexcept;

  ///
  /// Generate and assign a new Universally Unique ID (UUID) to this
  /// object and any stored subobjects that also have a UUID.
  ///
  /// Though automatically assigned on construction, it can be manually set.
  ///
  virtual void setUUID();

  ///
  /// Manually assign Universally Unique ID (UUID).
  ///
  /// Though automatically assigned on construction, it can be manually set.
  ///
  void setUUID(UUID X);

  ///
  /// Retrieve the Node's Universally Unique ID (UUID).
  ///
  UUID getUUID() const;

  Kind getKind() const { return K; }
  static bool classof(const Node *N) { return N->getKind() == Kind::Node; }

protected:
  Node(Kind K);

private:
  Kind K;
  UUID Uuid;

  static std::map<UUID, Node*> UuidMap;
};

///
/// Return the text representation of a UUID.
///
std::string uuidToString(const UUID& Uuid);

///
/// Create UUID from text representation.
///
UUID uuidFromString(const std::string& Text);

} // namespace gtirb
