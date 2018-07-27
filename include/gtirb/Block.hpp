#pragma once

#include <gtirb/Export.hpp>
#include <gtirb/Instruction.hpp>
#include <gtirb/Node.hpp>
#include <vector>

namespace proto {
class Block;
}

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
  Block(EA startingAddress, EA endingAddress);

  ///
  /// Construct a block with some instructions.
  /// Instructions are copied and added as children.
  ///
  Block(EA startingAddress, EA endingAddress, std::vector<Instruction>&& instructions);

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

  EA getStartingAddress() const;
  EA getEndingAddress() const;

  std::vector<Instruction>& getInstructions();
  const std::vector<Instruction>& getInstructions() const;

  using MessageType = proto::Block;
  void toProtobuf(MessageType* message) const;
  void fromProtobuf(const MessageType& message);

private:
  EA startingAddress{};
  EA endingAddress{};
  std::vector<Instruction> instructions;
};
}
