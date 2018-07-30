#pragma once

#include <boost/uuid/uuid.hpp>
#include <functional>
#include <gsl/gsl>
#include <gtirb/Export.hpp>
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
  ///
  /// Retrieve a node by its UUID.
  ///
  /// \return node with the given UUID, or nullptr if none exists.
  static Node* getByUUID(UUID uuid);

  ///
  /// Automatically assigns the Node a UUID.
  ///
  Node();

  ///
  /// Copy a node, assigning a new UUID to the copy.
  ///
  explicit Node(const Node&);

  ///
  /// Nodes are not copyable due to UUIDs.
  ///
  Node& operator=(const Node&) = delete;

  ///
  /// Move constructor. Clears the UUID of the moved node and updates
  /// the UUID to node mapping.
  ///
  Node(Node&& other) noexcept;

  ///
  /// Move assignment. Clears the UUID of the moved node and updates
  /// the UUID to node mapping.
  ///
  Node& operator=(Node&& other) noexcept;

  ///
  /// This will serve as a base class for other nodes.
  ///
  virtual ~Node() noexcept;

  ///
  /// Generate and assign a new Universally Unique ID (UUID).
  ///
  /// Though automatically assigned on construction, it can be manually set.
  ///
  void setUUID();

  ///
  /// Manually assign Universally Unique ID (UUID).
  ///
  /// Though automatically assigned on construction, it can be manually set.
  ///
  void setUUID(UUID x);

  ///
  /// Retrieve the Node's Universally Unique ID (UUID).
  ///
  UUID getUUID() const;

private:
  UUID uuid;

  static std::map<UUID, Node*> uuidMap;
};
} // namespace gtirb
