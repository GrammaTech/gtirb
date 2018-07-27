#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <set>

namespace proto {
class Instruction;
}

namespace gtirb {
class Module;

///
/// \class Instruction
///
/// Instructions have two fields, a pointer to the raw bytes of
/// the instruction (including any instruction prefix) and a list
/// of pairs of operand indices and associated pointers to
/// symbolic information for that operand.
///
/// Byte pointers are offsets into the ImageByteMap.
///
class GTIRB_EXPORT_API Instruction : public Node {
public:
  ///
  /// Default Constructor.
  ///
  Instruction() = default;
  Instruction(EA ea, uint64_t size = 0);

  ///
  /// Copy constructor. Assigns a new UUID to the copy.
  ///
  explicit Instruction(const Instruction& other) = default;

  ///
  /// Move constructor
  ///
  Instruction(Instruction&&) = default;

  ///
  /// Move assignment
  ///
  Instruction& operator=(Instruction&&) = default;

  void setEA(gtirb::EA x);
  gtirb::EA getEA() const;

  uint64_t getSize() const;

  std::vector<uint8_t> getBytes(const Module& module) const;

  using MessageType = proto::Instruction;
  void toProtobuf(MessageType* message) const;
  void fromProtobuf(const MessageType& message);

private:
  EA ea{0};
  uint64_t size{0};
};
}
