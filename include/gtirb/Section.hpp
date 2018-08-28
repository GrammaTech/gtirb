#ifndef GTIRB_SECTION_H
#define GTIRB_SECTION_H

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
public:
  Section() = default;
  Section(std::string N, EA Address, uint64_t Size);

  ///
  /// Copy constructor. Assigns a new UUID to the copy.
  ///
  explicit Section(const Section& Other) = default;

  ///
  /// Move constructor
  ///
  Section(Section&&) = default;

  ///
  /// Move assignment
  ///
  Section& operator=(Section&&) = default;

  bool operator==(const Section& Other) const;
  bool operator!=(const Section& Other) const;

  const std::string& getName() const;
  const EA getAddress() const;
  uint64_t getSize() const;

  using MessageType = proto::Section;
  void toProtobuf(MessageType* Message) const;
  void fromProtobuf(const MessageType& Message);

private:
  std::string Name;
  EA Address{0};
  uint64_t Size{0};
};
} // namespace gtirb

#endif // GTIRB_SECTION_H
