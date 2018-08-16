#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <cstdint>

namespace proto {
class Section;
}
namespace gtirb {
///
/// \class Section
class GTIRB_EXPORT_API Section : public Node {
  Section() = default;
  Section(const std::string &Name, EA Address, uint64_t Size)
      : Node(), Name(Name), Address(Address), Size(Size) {}

public:
  static Section* Create(Context& C) { return new (C) Section; }
  static Section* Create(Context& C, const std::string& Name, EA Address,
                         uint64_t Size) {
    return new (C) Section(Name, Address, Size);
  }

  bool operator==(const Section& Other) const;
  bool operator!=(const Section& Other) const;

  const std::string& getName() const;
  const EA getAddress() const;
  uint64_t getSize() const;

  using MessageType = proto::Section;
  void toProtobuf(MessageType* Message) const;
  static Section *fromProtobuf(Context &C, const MessageType& Message);

private:
  std::string Name;
  EA Address{0};
  uint64_t Size{0};
};
} // namespace gtirb
