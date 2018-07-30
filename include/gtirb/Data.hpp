#pragma once

#include <gtirb/EA.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Node.hpp>

namespace proto {
class Data;
}
namespace gtirb {
///
/// \class Data
///
/// Represents a data object, possibly symbolic. Does not directly store
/// the data bytes, which are kept in the ImageByteMap.
///
/// \note
/// This seems very similar to the Instruction class since each
/// piece of data basically holds a pointer to bytes and some
/// amount of symoblic information which may be comprised as a
/// simple mathematical expression combining symobls.  The only
/// addition is that data should have a `size` field (any other
/// information like types should be stored in external tables).
///
/// Perhaps data and instruction should share a base class which
/// provides the byte-pointer and symbolic expression support?
///
class GTIRB_EXPORT_API Data : public Node {
public:
  // Default constructor required for serialization.
  Data() = default;

  ///
  /// Copy constructor. Assigns a new UUID to the copy.
  ///
  explicit Data(const Data& other) = default;
  ///
  /// Move constructor
  ///
  Data(Data&&) = default;

  ///
  /// Move assignment
  ///
  Data& operator=(Data&&) = default;

  Data(EA address_, uint64_t size_) : address(address_), size(size_) {}

  EA getAddress() const;

  uint64_t getSize() const;

  std::vector<uint8_t> getBytes(const Module& module) const;

  using MessageType = proto::Data;
  void toProtobuf(MessageType* message) const;
  void fromProtobuf(const MessageType& message);

private:
  EA address{0};
  uint64_t size{0};
};
} // namespace gtirb
