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

///
/// \class Block
///
/// \brief A basic block.
///
class GTIRB_EXPORT_API Block : public Node {
  Block() : Node(Kind::Block) {}

  ///
  /// Construct an empty block
  ///
  Block(Addr Addr, uint64_t S, uint64_t Decode)
      : Node(Kind::Block), Address(Addr), Size(S), DecodeMode(Decode) {}

public:

  static Block *Create(Context &C) { return new (C) Block; }
  static Block *Create(Context &C, EA Address, uint64_t Size,
                       uint64_t DecodeMode = 0) {
    return new (C) Block(Address, Size, DecodeMode);
  }


  /// DOCFIXME[check all]
  /// \brief Get the address from a \ref Block.
  ///
  /// \return The address.
  ///
  Addr getAddress() const { return Address; }

  /// DOCFIXME[check all]
  /// \brief Get the size from a \ref Block.
  ///
  /// \return The size DOCFIXME[defined how?]
  ///
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
  /// \return DOCFIXME
  ///
  void toProtobuf(MessageType* Message) const;


  /// \brief DOCFIXME
  ///
  /// \param C DOCFIXME
  ///
  /// \param Message DOCFIXME
  ///
  /// \return DOCFIXME
  ///
  static Block *fromProtobuf(Context &C, const MessageType& Message);

  static bool classof(const Node *N) { return N->getKind() == Kind::Block; }

private:
  Addr Address;
  uint64_t Size{0};
  uint64_t DecodeMode{0};
};


/// \class InstructionRef
///
/// \brief Describes the location of an instruction.
///
struct GTIRB_EXPORT_API InstructionRef {
  NodeRef<Block> BlockRef; ///< The block to which the instruction belongs. [DOCFIXME[check]
  uint64_t Offset;         ///< The offset of the instruction in the block. DOCFIXME[in what? instructions? bytes?]


  /// \brief DOCFIXME
  using MessageType = proto::InstructionRef;

  /// \brief DOCFIXME
  ///
  /// \param message DOCFIXME
  ///
  /// \return DOCFIXME
  ///
  void toProtobuf(MessageType* message) const;


  /// \brief DOCFIXME
  ///
  /// \param message DOCFIXME
  ///
  /// \return DOCFIXME
  ///
  void fromProtobuf(Context &, const MessageType& message);
};

} // namespace gtirb

#endif // GTIRB_BLOCK_H
