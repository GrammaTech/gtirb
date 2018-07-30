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
/// Instructions store offsets into the ImageByteMap. They do not store their
/// bytes directly.
///
class GTIRB_EXPORT_API Instruction : public Node {
public:
  ///
  /// Default Constructor.
  ///
  Instruction() = default;
  Instruction(EA address, uint64_t size = 0);

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

  void setAddress(gtirb::EA x);
  gtirb::EA getAddress() const;

  uint64_t getSize() const;

  std::vector<uint8_t> getBytes(const Module& module) const;

  using MessageType = proto::Instruction;
  void toProtobuf(MessageType* message) const;
  void fromProtobuf(const MessageType& message);

private:
  EA address{0};
  uint64_t size{0};
};
} // namespace gtirb
