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
public:
  /// \brief Default constructor
  Block() = default;


  /// \brief Copy constructor.
  /// Assigns a new UUID to the copy.
  ///
  explicit Block(const Block&) = default;


  /// \brief Construct an empty block.
  ///
  /// \param Address    The address of the first program point in the block. DOCFIXME[check]
  ///
  /// \param Size       The size of the block, in DOCFIXME[bytes? program points?]
  ///
  /// \param DecodeMode DOCFIXME
  ///
  /// \return A new, empty block DOCFIXME
  Block(Addr Address, uint64_t Size, uint64_t DecodeMode = 0);

  ///
  /// \brief Move constructor.
  ///
  Block(Block&&) = default;


  /// \brief Move assignment.
  ///
  Block& operator=(Block&&) = default;


  /// \brief Defaulted trivial destructor.
  ///
  ~Block() override = default;


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
  /// \param Message DOCFIXME
  ///
  /// \return DOCFIXME
  ///
  void fromProtobuf(const MessageType& Message);

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
  void fromProtobuf(const MessageType& message);
};

} // namespace gtirb

#endif // GTIRB_BLOCK_H
