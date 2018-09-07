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
/// \brief Represents a data object, possibly symbolic.
///
/// Does not directly store the data bytes, which are kept in the
/// \ref ImageByteMap.
///
class GTIRB_EXPORT_API DataObject : public Node {
  DataObject(Context& C) : Node(C, Kind::DataObject) {}

  DataObject(Context& C, Addr A, uint64_t S)
      : Node(C, Kind::DataObject), Address(A), Size(S) {}

public:
  static DataObject* Create(Context& C) { return new (C) DataObject(C); }
  static DataObject* Create(Context& C, Addr Address, uint64_t Size) {
    return new (C) DataObject(C, Address, Size);
  }

  /// \brief Get the address of DOCFIXME.
  ///
  /// \return The address.
  ///
  Addr getAddress() const { return Address; }

  /// \brief Get the size of DOCFIXME.
  ///
  /// \return The size.
  ///
  uint64_t getSize() const { return Size; }

  /// \brief DOCFIXME
  using MessageType = proto::DataObject;

  /// \brief DOCFIXME
  ///
  /// \param Message DOCFIXME
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief DOCFIXME
  ///
  /// \param C DOCFIXME
  /// \param Message DOCFIXME
  ///
  /// \return The deserialized DataObject object, or null on failure.
  static DataObject* fromProtobuf(Context& C, const MessageType& Message);

  static bool classof(const Node* N) {
    return N->getKind() == Kind::DataObject;
  }

private:
  Addr Address{0};
  uint64_t Size{0};
};
} // namespace gtirb

#endif // GTIRB_DATAOBJECT_H
