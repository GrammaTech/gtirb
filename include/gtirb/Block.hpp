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
public:
  // Default constructor
  Block() = default;

  ///
  /// Copy constructor. Assigns a new UUID to the copy.
  ///
  explicit Block(const Block& other) = default;
  ///
  /// Construct an empty block
  ///
  Block(EA address, uint64_t size);

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

  EA getAddress() const;
  uint64_t getSize() const;

  using MessageType = proto::Block;
  void toProtobuf(MessageType* message) const;
  void fromProtobuf(const MessageType& message);

private:
  EA address{};
  uint64_t size{0};
};

///
/// \class InstructionRef
///
/// Describes the location of an instruction.
///
struct GTIRB_EXPORT_API InstructionRef {
  NodeRef<Block> block;
  uint64_t offset;

  using MessageType = proto::InstructionRef;
  void toProtobuf(MessageType* message) const;
  void fromProtobuf(const MessageType& message);
};

// TODO:
// serialization
// add to Tables

} // namespace gtirb
