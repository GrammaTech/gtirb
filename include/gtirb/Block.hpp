//===- Block.hpp ------------------------------------------------*- C++ -*-===//
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
#ifndef GTIRB_BLOCK_H
#define GTIRB_BLOCK_H

#include <gtirb/Addr.hpp>
#include <gtirb/Export.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/NodeRef.hpp>
#include <proto/Block.pb.h>
#include <cstdint>
#include <vector>

/// \file Block.hpp
/// \brief Classes gtirb::Block and gtirb::InstructionRef.

namespace proto {
class InstructionRef;
} // namespace proto

namespace gtirb {

/// \class Block
///
/// \brief A basic block.
/// \see \ref CFG_GROUP
class GTIRB_EXPORT_API Block : public Node {
public:
  /// \enum Exit
  /// \brief Indicates how control flow exits a block.
  enum class Exit : uint8_t {
    Fallthrough = proto::Fallthrough,
    Branch = proto::Branch,
    Call = proto::Call,
    Return = proto::Return
  };

  /// \brief Create a Block object in its default state.
  ///
  /// \param C  The Context in which this object will be held.
  ///
  /// \return The newly created object.
  static Block* Create(Context& C) { return new (C) Block(C); }

  /// \brief Create a Block object.
  ///
  /// \param C          The Context in which this Block will be held.
  /// \param Address    The address where the Block is located.
  /// \param Size       The size of the Block in bytes.
  /// \param ExitKind   Indicates how control flow exits the block.
  /// \param DecodeMode The decode mode of the Block.

  ///
  /// \return The newly created Block.
  static Block* Create(Context& C, Addr Address, uint64_t Size,
                       Exit ExitKind = Exit::Fallthrough,
                       uint64_t DecodeMode = 0) {
    return new (C) Block(C, Address, Size, ExitKind, DecodeMode);
  }

  /// \brief Get the address from a \ref Block.
  ///
  /// \return The address.
  Addr getAddress() const { return Address; }

  /// \brief Get the size from a \ref Block.
  ///
  /// \return The size in bytes.
  uint64_t getSize() const { return Size; }

  /// \brief Get the decode mode from a \ref Block.
  ///
  /// \return The decode mode.
  uint64_t getDecodeMode() const { return DecodeMode; }

  /// \brief Get the exit kind from a \ref Block.
  ///
  /// \return The exit kind.
  Exit getExitKind() const { return ExitKind; }

  using MessageType = proto::Block;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a Block from a protobuf message.
  ///
  /// \param C   The Context in which the deserialized Block will be held.
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized Block object, or null on failure.
  static Block* fromProtobuf(Context& C, const MessageType& Message);

  /// \cond INTERNAL
  static bool classof(const Node* N) { return N->getKind() == Kind::Block; }
  /// \endcond

private:
  Block(Context& C) : Node(C, Kind::Block) {}
  Block(Context& C, Addr Addr, uint64_t S, Exit E, uint64_t Decode)
      : Node(C, Kind::Block), Address(Addr), Size(S), DecodeMode(Decode),
        ExitKind(E) {}

  Addr Address;
  uint64_t Size{0};
  uint64_t DecodeMode{0};
  Exit ExitKind{Exit::Fallthrough};
};

/// \class InstructionRef
///
/// \brief Describes the location of an instruction.
struct GTIRB_EXPORT_API InstructionRef {

  /// \brief The block in which the instruction is located.
  NodeRef<Block> BlockRef;

  /// \brief The offset of the instruction from the start of tyhe block, in
  /// bytes.
  uint64_t Offset;

  /// \brief The protobuf message type used for serializing InstructionRef.
  using MessageType = proto::InstructionRef;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a InstructionRef from a protobuf message.
  ///
  /// \param C  The Context in which the deserialized InstructionRef will be
  ///           held.
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized InstructionRef object, or null on failure.
  void fromProtobuf(Context& C, const MessageType& Message);
};

} // namespace gtirb

#endif // GTIRB_BLOCK_H
