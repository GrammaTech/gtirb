//===- DataObject.hpp -------------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2018 GrammaTech, Inc.
//
//  This code is licensed under the MIT license. See the LICENSE file in the
//  project root for license terms.
//
//  This project is sponsored by the Office of Naval Research, One Liberty
//  Center, 875 N. Randolph Street, Arlington, VA 22203 under contract #
//  N68335-17-C-0700.  The content of the information does not necessarily
//  reflect the position or policy of the Government and no official
//  endorsement should be inferred.
//
//===----------------------------------------------------------------------===//
#ifndef GTIRB_DATAOBJECT_H
#define GTIRB_DATAOBJECT_H

#include <gtirb/Addr.hpp>
#include <gtirb/Node.hpp>
#include <cstdint>
#include <vector>

/// \file DataObject.hpp
/// \brief Class gtirb::DataObject.

namespace proto {
class DataObject;
}
namespace gtirb {
///
/// \class DataObject
///
/// \brief Represents a data object, possibly symbolic.
///
/// Does not directly store the data bytes, which are kept in the
/// \ref ImageByteMap.
///
class GTIRB_EXPORT_API DataObject : public Node {

  DataObject(Context& C) : Node(C, Kind::DataObject) {}

  DataObject(Context& C, Addr A, uint64_t S)
      : Node(C, Kind::DataObject), Address(A), Size(S) {}

public:
  /// \brief Create a DataObject object in its default state.
  ///
  /// \param C  The Context in which the newly-created DataObject will
  /// be held.
  ///
  /// \return The newly created DataObject.
  static DataObject* Create(Context& C) { return C.Create<DataObject>(C); }

  /// \brief Create a DataObject object.
  ///
  /// \param C The Context in which the newly-created DataObject will
  /// be held.
  ///
  /// \param Address  The initial address of the object.
  ///
  /// \param Size     The size of the object in bytes.
  ///
  /// \return The newly created DataObject.
  static DataObject* Create(Context& C, Addr Address, uint64_t Size) {
    return C.Create<DataObject>(C, Address, Size);
  }

  /// \brief Get the address of a DataObject.
  ///
  /// \return The address.
  ///
  Addr getAddress() const { return Address; }

  /// \brief Get the size of a DataObject.
  ///
  /// \return The size.
  ///
  uint64_t getSize() const { return Size; }

  /// @cond INTERNAL
  /// \brief The protobuf message type used for serializing DataObject.
  using MessageType = proto::DataObject;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a DataObject from a protobuf message.
  ///
  /// \param C   The Context in which the deserialized DataObject will be held.
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized DataObject object, or null on failure.
  static DataObject* fromProtobuf(Context& C, const MessageType& Message);

  static bool classof(const Node* N) {
    return N->getKind() == Kind::DataObject;
  }
  /// @endcond

private:
  Addr Address{0};
  uint64_t Size{0};

  friend class Context;
};
} // namespace gtirb

#endif // GTIRB_DATAOBJECT_H
