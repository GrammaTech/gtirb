#ifndef GTIRB_NODEREF_H
#define GTIRB_NODEREF_H

#include <gtirb/Node.hpp>

namespace gtirb {

/// \class NodeRef
///
/// \brief Reference a Node by UUID.
///
/// \tparam NodeT DOCFIXME  
///
/// This behaves like a pointer, but it looks up the node by its UUID, which
/// is persistent across serialization, moves, etc.
template <typename NodeT> class NodeRef {
public:

  /// \brief Default constructor.
  NodeRef() = default;


  /// \brief Constructor.
  ///
  /// \param Uuid_ The UUID to reference DOCFIXME[check].
  ///
  NodeRef(const UUID& Uuid_) : Uuid(Uuid_) {}


  /// \brief Constructor
  ///
  /// \param node The Node to reference DOCFIXME[check].
  ///
  NodeRef(const NodeT& node) : Uuid(node.getUUID()){};


  NodeRef(const NodeT* node) : NodeRef(*node) {}


  /// \brief Copy constructor.
  NodeRef(const NodeRef&) = default;


  /// \brief Move constructor
  NodeRef(NodeRef&&) = default;


  /// \brief Move assignment operator overload
  NodeRef& operator=(NodeRef&&) = default;


  /// \brief Assignment operator overload.
  NodeRef& operator=(const NodeRef&) = default;


  /// \brief Cast to pointer.
  operator NodeT*() { return this->get(); }


  /// \brief Cast to boolean.
  operator bool() const { return this->get(); }


  /// \brief Dereference operator overload.
  const NodeT& operator*() const { return *this->get(); }


  /// \brief Dereference operator overload.
  NodeT& operator*() { return *this->get(); }


  /// \brief Member access operator overload.
  const NodeT* operator->() const { return this->get(); }


  /// \brief Member access operator overload.
  NodeT* operator->() { return this->get(); }


  /// \brief Get the associated UUID.
  ///
  /// \return The UUID.
  UUID getUUID() const { return this->Uuid; }

private:
  UUID Uuid;

  NodeT* get() const {
    return dyn_cast_or_null<NodeT>(Node::getByUUID(this->Uuid));
  }
};
} // namespace gtirb

#endif // GTIRB_NODEREF_H
