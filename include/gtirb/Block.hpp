#pragma once

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
/// A basic block.
///
class GTIRB_EXPORT_API Block : public Node {
public:
  // Default constructor
  Block() = default;

  ///
  /// Copy constructor. Assigns a new UUID to the copy.
  ///
  explicit Block(const Block&) = default;
  ///
  /// Construct an empty block
  ///
  Block(Addr Address, uint64_t Size, uint64_t DecodeMode = 0);

  ///
  /// Move constructor
  ///
  Block(Block&&) = default;

  ///
  /// Move assignment
  ///
  Block& operator=(Block&&) = default;

  ///
  /// Defaulted trivial destructor.
  ///
  ~Block() override = default;

  Addr getAddress() const;
  uint64_t getSize() const;
  uint64_t getDecodeMode() const;

  using MessageType = proto::Block;
  void toProtobuf(MessageType* Message) const;
  void fromProtobuf(const MessageType& Message);

private:
  Addr Address{};
  uint64_t Size{0};
  uint64_t DecodeMode{0};
};

///
/// \class InstructionRef
///
/// Describes the location of an instruction.
///
struct GTIRB_EXPORT_API InstructionRef {
  NodeRef<Block> BlockRef;
  uint64_t Offset;

  using MessageType = proto::InstructionRef;
  void toProtobuf(MessageType* message) const;
  void fromProtobuf(const MessageType& message);
};

} // namespace gtirb
