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
  Section() : Node(Kind::Section) {}
  Section(const std::string &N, EA A, uint64_t S)
      : Node(Kind::Section), Name(N), Address(A), Size(S) {}

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

  static bool classof(const Node *N) { return N->getKind() == Kind::Section; }

private:
  std::string Name;
  EA Address{0};
  uint64_t Size{0};
};
} // namespace gtirb
