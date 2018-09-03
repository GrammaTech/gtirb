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
public:
  /// \brief Default constructor required for serialization.
  DataObject() = default;



  /// \brief Constructor DOCFIXME
  ///
  /// \param  Address_  DOCFIXME
  /// \param  Size_     DOCFIXME
  ///
  DataObject(Addr Address_, uint64_t Size_) : Address(Address_), Size(Size_) {}


  /// \brief Copy constructor.
  /// Assigns a new UUID to the copy.
  ///
  explicit DataObject(const DataObject&) = default;


  /// \brief Move constructor.
  ///
  DataObject(DataObject&&) = default;


  /// \brief Move assignment.
  ///
  DataObject& operator=(DataObject&&) = default;


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
  /// \param Message DOCFIXME
  ///
  /// \return void
  void fromProtobuf(const MessageType& Message);

private:
  Addr Address{0};
  uint64_t Size{0};
};
} // namespace gtirb

#endif // GTIRB_DATAOBJECT_H
