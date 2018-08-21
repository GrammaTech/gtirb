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
  DataObject() : Node(Kind::DataObject) {}

  DataObject(EA A, uint64_t S)
      : Node(Kind::DataObject), Address(A), Size(S) {}

public:
  static DataObject *Create(Context &C) { return new (C) DataObject; }
  static DataObject *Create(Context &C, EA Address, uint64_t Size) {
    return new (C) DataObject(Address, Size);
  }

  EA getAddress() const;

  uint64_t getSize() const;

  using MessageType = proto::DataObject;
  void toProtobuf(MessageType* Message) const;
  static DataObject *fromProtobuf(Context &C, const MessageType& Message);

  static bool classof(const Node* N) {
    return N->getKind() == Kind::DataObject;
  }

private:
  EA Address{0};
  uint64_t Size{0};
};
} // namespace gtirb
