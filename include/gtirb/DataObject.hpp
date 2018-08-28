#ifndef GTIRB_DATAOBJECT_H
#define GTIRB_DATAOBJECT_H

#include <gtirb/Addr.hpp>
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

  DataObject(Addr Address_, uint64_t Size_) : Address(Address_), Size(Size_) {}

  ///
  /// Copy constructor. Assigns a new UUID to the copy.
  ///
  explicit DataObject(const DataObject&) = default;
  ///
  /// Move constructor
  ///
  DataObject(DataObject&&) = default;

  ///
  /// Move assignment
  ///
  DataObject& operator=(DataObject&&) = default;

  Addr getAddress() const { return Address; }

  uint64_t getSize() const { return Size; }

  using MessageType = proto::DataObject;
  void toProtobuf(MessageType* Message) const;
  void fromProtobuf(const MessageType& Message);

private:
  Addr Address{0};
  uint64_t Size{0};
};
} // namespace gtirb

#endif // GTIRB_DATAOBJECT_H
