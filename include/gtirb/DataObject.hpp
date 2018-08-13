#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <cstdint>
#include <vector>

namespace proto {
class DataObject;
}
namespace gtirb {
///
/// \class DataObject
///
/// Represents a data object, possibly symbolic. Does not directly store
/// the data bytes, which are kept in the ImageByteMap.
///
class GTIRB_EXPORT_API DataObject : public Node {
public:
  // Default constructor required for serialization.
  DataObject() = default;

  DataObject(EA address_, uint64_t size_) : address(address_), size(size_) {}

  ///
  /// Copy constructor. Assigns a new UUID to the copy.
  ///
  explicit DataObject(const DataObject& other) = default;
  ///
  /// Move constructor
  ///
  DataObject(DataObject&&) = default;

  ///
  /// Move assignment
  ///
  DataObject& operator=(DataObject&&) = default;

  EA getAddress() const;

  uint64_t getSize() const;

  using MessageType = proto::DataObject;
  void toProtobuf(MessageType* message) const;
  void fromProtobuf(const MessageType& message);

private:
  EA address{0};
  uint64_t size{0};
};
} // namespace gtirb
