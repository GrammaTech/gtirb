#pragma once

#include <cstdint>
#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>

namespace proto {
class Section;
}
namespace gtirb {
///
/// \class Section
class GTIRB_EXPORT_API Section : public Node {
public:
  Section() = default;
  Section(std::string n, EA address, uint64_t size);

  ///
  /// Copy constructor. Assigns a new UUID to the copy.
  ///
  explicit Section(const Section& other) = default;

  ///
  /// Move constructor
  ///
  Section(Section&&) = default;

  ///
  /// Move assignment
  ///
  Section& operator=(Section&&) = default;

  bool operator==(const Section& other) const;
  bool operator!=(const Section& other) const;

  const std::string& getName() const;
  const EA getAddress() const;
  const uint64_t getSize() const;

  using MessageType = proto::Section;
  void toProtobuf(MessageType* message) const;
  void fromProtobuf(const MessageType& message);

private:
  std::string name;
  EA address{0};
  uint64_t size{0};
};
}
