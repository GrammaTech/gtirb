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
#include <gtirb/CFG.hpp>
#include <gtirb/Export.hpp>
#include <gtirb/Node.hpp>
#include <proto/Block.pb.h>
#include <cstdint>
#include <vector>

/// \file Block.hpp
/// \ingroup CFG_GROUP
/// \brief Classes gtirb::Block and gtirb::InstructionRef.
/// \see CFG_GROUP

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
  /// \brief Create a Block object.
  ///
  /// \param Vertex   The CFG vertex corresponding to this node.
  /// \param C          The Context in which this Block will be held.
  /// \param Address    The address where the Block is located.
  /// \param Size       The size of the Block in bytes.
  /// \param DecodeMode The decode mode of the Block.
  ///
  /// \return The newly created Block.
  static Block* Create(Context& C, CFG::vertex_descriptor Vertex, Addr Address,
                       uint64_t Size, uint64_t DecodeMode = 0) {
    return C.Create<Block>(C, Address, Size, Vertex, DecodeMode);
  }

  /// \brief Get the address from a \ref Block.
  ///
  /// \return The address of the start of the block.
  ///
  /// Use with Block::getSize() to obtain arguments to pass to
  /// ByteMap::data() for an iterator over the contents of a \ref Block.
  Addr getAddress() const { return Address; }

  /// \brief Get the size from a \ref Block.
  ///
  /// \return The size in bytes.
  ///
  /// Use with Block::getAddress() to obtain arguments to pass to
  /// ByteMap::data() for an iterator over the contents of a \ref Block.
  uint64_t getSize() const { return Size; }

  /// \brief Get a vertex descriptor which can be used to locate the Block
  /// in the CFG.
  ///
  /// \return The vertex descriptor.
  CFG::vertex_descriptor getVertex() const { return Vertex; };

  /// \brief Get the decode mode from a \ref Block.
  ///
  /// \return The decode mode.
  uint64_t getDecodeMode() const { return DecodeMode; }

  /// \brief The protobuf message type used for serializing Block.
  using MessageType = proto::Block;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \cond INTERNAL
  static bool classof(const Node* N) { return N->getKind() == Kind::Block; }
  /// \endcond

private:
  Block(Context& C) : Node(C, Kind::Block) {}
  Block(Context& C, Addr Addr, uint64_t S, CFG::vertex_descriptor V,
        uint64_t Decode)
      : Node(C, Kind::Block), Address(Addr), Size(S), Vertex(V),
        DecodeMode(Decode) {}

  Addr Address;
  uint64_t Size{0};
  CFG::vertex_descriptor Vertex;
  uint64_t DecodeMode{0};

  friend class Context;
};

/// \ingroup CFG_GROUP
/// \brief Create a new basic block and add it to the control-flow graph.
///
/// \tparam Ts   Types of forwarded arguments.
///
/// \param Cfg   The control-flow graph to modify
/// \param C     The Context in which the Block will be held.
/// \param Args  Forwarded to Block::Create()
///
/// \return A descriptor which can be used to retrieve the \ref Block.
template <class... Ts> Block* emplaceBlock(CFG& Cfg, Context& C, Ts&&... Args) {
  auto Descriptor = add_vertex(Cfg);
  auto* B = Block::Create(C, Descriptor, std::forward<Ts>(Args)...);
  Cfg[Descriptor] = B;
  return B;
}

/// \class InstructionRef
///
/// \brief Describes the location of an instruction.
struct GTIRB_EXPORT_API InstructionRef {

  /// \brief The UUID of the block in which the instruction is located.
  UUID BlockId;

  /// \brief The offset of the instruction from the start of the block, in
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
