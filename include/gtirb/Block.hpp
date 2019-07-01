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
#include <gtirb/CfgNode.hpp>
#include <gtirb/Export.hpp>
#include <gtirb/Node.hpp>
#include <proto/Block.pb.h>
#include <cstdint>
#include <vector>

/// \file Block.hpp
/// \ingroup CFG_GROUP
/// \brief Classes gtirb::Block and gtirb::Offset.
/// \see CFG_GROUP

namespace proto {
class Offset;
} // namespace proto

namespace gtirb {
/// \class Block
///
/// \brief A basic block.
/// \see \ref CFG_GROUP
class GTIRB_EXPORT_API Block : public CfgNode {
public:
  /// \brief Create a Block object.
  ///
  /// \param C          The Context in which this Block will be held.
  /// \param Address    The address where the Block is located.
  /// \param Size       The size of the Block in bytes.
  /// \param DecodeMode The decode mode of the Block.
  ///
  /// \return The newly created Block.
  static Block* Create(Context& C, Addr Address, uint64_t Size,
                       uint64_t DecodeMode = 0) {
    return C.Create<Block>(C, Address, Size, DecodeMode);
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

  /// \brief Get the decode mode from a \ref Block.
  ///
  /// \return The decode mode.
  uint64_t getDecodeMode() const { return DecodeMode; }

  /// @cond INTERNAL
  /// \brief The protobuf message type used for serializing Block.
  using MessageType = proto::Block;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a Block from a protobuf message.
  ///
  /// \param C  The Context in which the deserialized Block will be held.
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized Block object, or null on failure.
  static Block* fromProtobuf(Context& C, const MessageType& Message);

  static bool classof(const Node* N) { return N->getKind() == Kind::Block; }
  /// @endcond

private:
  Block(Context& C) : CfgNode(C, Kind::Block) {}
  Block(Context& C, Addr Addr, uint64_t S, uint64_t Decode)
      : CfgNode(C, Kind::Block), Address(Addr), Size(S), DecodeMode(Decode) {}

  Addr Address;
  uint64_t Size{0};
  uint64_t DecodeMode{0};

  friend class Context;
};

/// \class Offset
///
/// \brief Describes a location inside a block or data object.
struct GTIRB_EXPORT_API Offset {

  /// \brief The UUID of the block or data object.
  UUID ElementId;

  /// \brief The displacement from the start of the block or data object, in
  /// bytes.
  uint64_t Displacement;

  /// \brief Constructor using a uuid and a displacement.
  Offset(const UUID& elementId, const uint64_t displacement)
      : ElementId(elementId), Displacement(displacement) {}

  /// \brief Default constructor.
  Offset() {}

  /// @cond INTERNAL
  /// \brief The protobuf message type used for serializing Offset.
  using MessageType = proto::Offset;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a Offset from a protobuf message.
  ///
  /// \param C  The Context in which the deserialized Offset will be
  ///           held.
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized Offset object, or null on failure.
  void fromProtobuf(Context& C, const MessageType& Message);
  /// @endcond

  /// \brief Equality operator for \ref Offset.
  // Note:boost uiid is not constexpr
  friend bool operator==(const Offset& LHS, const Offset& RHS) noexcept {
    return LHS.ElementId == RHS.ElementId &&
           LHS.Displacement == RHS.Displacement;
  }

  /// \brief Inequality operator for \ref Offset.
  friend bool operator!=(const Offset& LHS, const Offset& RHS) noexcept {
    return !operator==(LHS, RHS);
  }

  /// \brief Less-than operator for \ref Offset.
  friend constexpr bool operator<(const Offset& LHS,
                                  const Offset& RHS) noexcept {
    return std::tie(LHS.ElementId, LHS.Displacement) <
           std::tie(RHS.ElementId, RHS.Displacement);
  }

  /// \brief Greater-than operator for \ref Offset.
  friend constexpr bool operator>(const Offset& LHS,
                                  const Offset& RHS) noexcept {
    return operator<(RHS, LHS);
  }

  /// \brief Less-than-or-equal operator for \ref Offset.
  friend constexpr bool operator<=(const Offset& LHS,
                                   const Offset& RHS) noexcept {
    return !operator<(RHS, LHS);
  }

  /// \brief Greater-than-or-equal operator for \ref Offset.
  friend constexpr bool operator>=(const Offset& LHS,
                                   const Offset& RHS) noexcept {
    return !operator<(LHS, RHS);
  }
};

} // namespace gtirb

namespace std {
// hash implementations of UUID and Offset

template <> struct hash<boost::uuids::uuid> {
  size_t operator()(const boost::uuids::uuid& uid) {
    return boost::hash<boost::uuids::uuid>()(uid);
  }
};
/// \bried Hash operation for \ref Offset.
template <> struct hash<gtirb::Offset> {
  size_t operator()(const gtirb::Offset& x) const {
    std::size_t h1 = hash<gtirb::UUID>{}(x.ElementId);
    std::size_t h2 = hash<uint64_t>{}(x.Displacement);
    return h1 ^ h2;
  }
};
} // namespace std

#endif // GTIRB_BLOCK_H
