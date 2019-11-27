//===- DataBlock.hpp -------------------------------------------*- C++ -*-===//
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
#ifndef GTIRB_DataBlock_H
#define GTIRB_DataBlock_H

#include <gtirb/Addr.hpp>
#include <gtirb/Node.hpp>
#include <cstdint>
#include <vector>

/// \file DataBlock.hpp
/// \brief Class gtirb::DataBlock.

namespace proto {
class DataBlock;
}
namespace gtirb {
///
/// \class DataBlock
///
/// \brief Represents a data object, possibly symbolic.
///
/// Does not directly store the data bytes, which are kept in the
/// \ref ImageByteMap.
///
class GTIRB_EXPORT_API DataBlock : public Node {

  DataBlock(Context& C) : Node(C, Kind::DataBlock) {}

  DataBlock(Context& C, uint64_t S) : Node(C, Kind::DataBlock), Size(S) {}

public:
  /// \brief Create a DataBlock object in its default state.
  ///
  /// \param C  The Context in which the newly-created DataBlock will
  /// be held.
  ///
  /// \return The newly created DataBlock.
  static DataBlock* Create(Context& C) { return C.Create<DataBlock>(C); }

  /// \brief Create a DataBlock object.
  ///
  /// \param C The Context in which the newly-created DataBlock will
  /// be held.
  ///
  /// \param Address  The initial address of the object.
  ///
  /// \param Size     The size of the object in bytes.
  ///
  /// \return The newly created DataBlock.
  static DataBlock* Create(Context& C, uint64_t Size) {
    return C.Create<DataBlock>(C, Size);
  }

  /// \brief Get the size of a DataBlock.
  ///
  /// \return The size.
  ///
  uint64_t getSize() const { return Size; }

  /// @cond INTERNAL
  /// \brief The protobuf message type used for serializing DataBlock.
  using MessageType = proto::DataBlock;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a DataBlock from a protobuf message.
  ///
  /// \param C   The Context in which the deserialized DataBlock will be held.
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized DataBlock object, or null on failure.
  static DataBlock* fromProtobuf(Context& C, const MessageType& Message);

  static bool classof(const Node* N) { return N->getKind() == Kind::DataBlock; }
  /// @endcond

private:
  uint64_t Size{0};

  friend class Context;
};
} // namespace gtirb

#endif // GTIRB_DataBlock_H
