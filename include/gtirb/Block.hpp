#pragma once

#include <gtirb/EA.hpp>
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
  Block() = default;

  ///
  /// Construct an empty block
  ///
  Block(EA Address, uint64_t Size, uint64_t DecodeMode)
      : Node(), Address(Address), Size(Size), DecodeMode(DecodeMode) {}

public:

  static Block *Create(Context &C) { return new (C) Block; }
  static Block *Create(Context &C, EA Address, uint64_t Size,
                       uint64_t DecodeMode = 0) {
    return new (C) Block(Address, Size, DecodeMode);
  }

  EA getAddress() const;
  uint64_t getSize() const;
  uint64_t getDecodeMode() const;

  using MessageType = proto::Block;
  void toProtobuf(MessageType* Message) const;
  static Block *fromProtobuf(Context &C, const MessageType& Message);

private:
  EA Address{};
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
  void fromProtobuf(Context &, const MessageType& message);
};

} // namespace gtirb
