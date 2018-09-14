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
#include <cstdint>
#include <vector>

namespace proto {
class Block;
class InstructionRef;
} // namespace proto

namespace gtirb {

/// \class Block
///
/// \brief A basic block.
class GTIRB_EXPORT_API Block : public Node {
  Block(Context& C) : Node(C, Kind::Block) {}
  Block(Context& C, Addr A, uint64_t S, uint64_t Decode)
      : Node(C, Kind::Block), Address(A), Size(S), DecodeMode(Decode) {}

public:
  /// \brief Create a Block object in its default state.
  ///
  /// \param C  The Context in which this object will be held.
  ///
  /// \return The newly created object.
  static Block* Create(Context& C) { return new (C) Block(C); }

  /// \brief Create a Block object.
  ///
  /// \param C        The Context in which this Block will be held.
  /// \param Address  DOCFIXME
  /// \param Size     DOCFIXME
  /// \param DecodeMode DOCFIXME
  ///
  /// \return The newly created Block.
  static Block* Create(Context& C, Addr Address, uint64_t Size,
                       uint64_t DecodeMode = 0) {
    return new (C) Block(C, Address, Size, DecodeMode);
  }

  /// DOCFIXME[check all]
  /// \brief Get the address from a \ref Block.
  ///
  /// \return The address.
  Addr getAddress() const { return Address; }

  /// DOCFIXME[check all]
  /// \brief Get the size from a \ref Block.
  ///
  /// \return The size DOCFIXME[defined how?]
  uint64_t getSize() const { return Size; }

  /// \brief Get the decode mode from a \ref Block.
  ///
  /// \return The decode mode.
  uint64_t getDecodeMode() const { return DecodeMode; }

  /// \brief DOCFIXME
  using MessageType = proto::Block;

  /// \brief DOCFIXME
  ///
  /// \param Message DOCFIXME
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief DOCFIXME
  ///
  /// \param C DOCFIXME
  /// \param Message DOCFIXME
  ///
  /// \return The deserialized Block object, or null on failure.
  static Block* fromProtobuf(Context& C, const MessageType& Message);

  static bool classof(const Node* N) { return N->getKind() == Kind::Block; }

private:
  Addr Address;
  uint64_t Size{0};
  uint64_t DecodeMode{0};
};

/// \class InstructionRef
///
/// \brief Describes the location of an instruction.
struct GTIRB_EXPORT_API InstructionRef {
  NodeRef<Block> BlockRef; ///< The block to which the instruction belongs.
                           ///< [DOCFIXME[check]
  uint64_t Offset; ///< The offset of the instruction in the block. DOCFIXME[in
                   ///< what? instructions? bytes?]

  /// \brief DOCFIXME
  using MessageType = proto::InstructionRef;

  /// \brief DOCFIXME
  ///
  /// \param Message DOCFIXME
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief DOCFIXME
  ///
  /// \param C DOCFIXME
  /// \param Message DOCFIXME
  ///
  /// \return void
  void fromProtobuf(Context& C, const MessageType& Message);
};

} // namespace gtirb

#endif // GTIRB_BLOCK_H
