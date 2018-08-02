#pragma once

#include <gtirb/ByteMap.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <proto/ImageByteMap.pb.h>
#include <array>
#include <boost/filesystem.hpp>
#include <gsl/gsl>
#include <set>
#include <type_traits>

namespace gtirb {
///
/// \class ImageByteMap
///
/// Contains the loaded raw image data for the module (binary).
///
class GTIRB_EXPORT_API ImageByteMap : public Node {
public:
  ImageByteMap() = default;

  ///
  /// Copy constructor. Assigns a new UUID to the copy.
  ///
  explicit ImageByteMap(const ImageByteMap& other) = default;

  ///
  /// Move constructor
  ///
  ImageByteMap(ImageByteMap&&) = default;

  ///
  /// Move assignment
  ///
  ImageByteMap& operator=(ImageByteMap&&) = default;

  ~ImageByteMap() override = default;

  ///
  /// \return     Sets the file name of the image.
  ///
  void setFileName(boost::filesystem::path x);

  ///
  /// \return     The loaded file name and path.
  ///
  boost::filesystem::path getFileName() const;

  ///
  /// Sets the base address of loaded file.
  ///
  void setBaseAddress(EA x);

  ///
  /// Gets the base addrress of loaded file.
  ///
  EA getBaseAddress() const;

  ///
  /// Sets the entry point of loaded file.
  ///
  void setEntryPointAddress(EA x);

  ///
  /// Gets the entry point of loaded file.
  ///
  EA getEntryPointAddress() const;

  ///
  /// If an invalid pair is passed in, the min and max will be set to an invalid state
  /// (gtirb::constants::BadAddress).  The range's min and max values are inclusive.
  ///
  /// \param      x   The minimum and maximum effective address (EA) for this Module.
  /// \return     False if the pair's first is > the pair's second.
  ///
  bool setEAMinMax(std::pair<gtirb::EA, gtirb::EA> x);

  ///
  /// Gets the minimum and maximum effective address (EA) for this Module.
  ///
  /// Check return values for gtirb::constants::BadAddress.
  ///
  /// \return     The minimum and maximum effective address (EA) for this Module.
  ///
  std::pair<gtirb::EA, gtirb::EA> getEAMinMax() const;

  ///
  ///
  ///
  void setRebaseDelta(int64_t x);

  ///
  ///
  ///
  int64_t getRebaseDelta() const;

  ///
  /// Marks the loaded image as having been relocated.
  ///
  /// This is primarily useful for loaders that load from sources that provide
  /// already-relocated content.
  ///
  void setIsRelocated();

  ///
  /// \return     True if the loaded image has been relocated.
  ///
  bool getIsRelocated() const;

  ///
  /// Sets byte map at the given address.
  ///
  /// The given address must be within the minimum and maximum EA.
  ///
  /// \throws std::out_of_range   Throws if the address to set data at is outside of the
  /// minimum and maximum EA.
  ///
  /// \param  ea      The address to store the data.
  /// \param  data    The data to store. This may be any POD type.
  ///
  /// \sa gtirb::ByteMap
  ///
  template <typename T> void setData(EA ea, const T& data) {
    static_assert(std::is_pod<T>::value, "T must be a POD type");
    this->byteMap.setData(ea, as_bytes(gsl::make_span(&data, 1)));
  }

  ///
  /// Sets byte map at the given address.
  ///
  /// The given address must be within the minimum and maximum EA.
  ///
  /// \throws std::out_of_range   Throws if the address to set data at is outside of the
  /// minimum and maximum EA.
  ///
  /// \param  ea      The address to store the data.
  /// \param  data    A pointer to the data to store (honoring Endianness).
  ///
  /// \sa gtirb::ByteMap
  ///
  void setData(EA ea, gsl::span<const gsl::byte> data);

  ///
  /// Sets byte map in the given range to a constant value.
  ///
  /// The given address must be within the minimum and maximum EA.
  ///
  /// \throws std::out_of_range   Throws if the address to set data at is outside of the
  /// minimum and maximum EA.
  ///
  /// \param  ea      The address to store the data.
  /// \param  data    A pointer to the data to store (honoring Endianness).
  ///
  /// \sa gtirb::ByteMap
  ///
  void setData(EA ea, size_t bytes, uint8_t value);

  ///
  /// Get data from the byte map at the given address.
  ///
  /// \param  x       The starting address for the data.
  /// \param  bytes   The number of bytes to read.
  ///
  /// \sa gtirb::ByteMap
  ///
  std::vector<uint8_t> getData(EA x, size_t bytes) const;

  ///
  /// Get data from the byte map at the given address.
  ///
  /// Returns an object of type T, initialized from the byte map data.
  /// T may be any POD type.
  ///
  /// \param  ea       The starting address for the data.
  ///
  /// \sa gtirb::ByteMap
  ///
  template <typename T> T getData(EA ea) {
    static_assert(std::is_pod<T>::value, "T must be a POD type");

    T result;
    auto destSpan = as_writeable_bytes(gsl::make_span(&result, 1));
    // Assign this to a variable so it isn't destroyed before we copy
    // from it (because gsl::span is non-owning).
    auto data = this->getData(ea, destSpan.size_bytes());
    auto srcSpan = as_bytes(gsl::make_span(data));
    assert(srcSpan.size() == destSpan.size());

    std::copy(srcSpan.begin(), srcSpan.end(), destSpan.begin());

    return result;
  }

  using MessageType = proto::ImageByteMap;
  void toProtobuf(MessageType* message) const;
  void fromProtobuf(const MessageType& message);

private:
  // Storage for the entire contents of the loaded image.
  gtirb::ByteMap byteMap;
  boost::filesystem::path fileName;
  std::pair<gtirb::EA, gtirb::EA> eaMinMax{};
  EA baseAddress{};
  EA entryPointAddress{};
  int64_t rebaseDelta{0};
  bool isRelocated{false};
};

///
/// Retrieve the bytes associated with an object.
///
/// Object can be any type which specifies a range of addresses via
/// getAddress() and getSize() methods (e.g. Data).
///
template <typename T> std::vector<uint8_t> getBytes(const ImageByteMap& byteMap, const T& object) {
  return byteMap.getData(object.getAddress(), object.getSize());
}

} // namespace gtirb
