#pragma once

#include <gtirb/ByteMap.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <proto/ImageByteMap.pb.h>
#include <array>
#include <boost/filesystem.hpp>
#include <gsl/gsl>
#include <set>

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
  /// Tests the byte map for empty.
  ///
  /// \return     True if the byte map is empty.
  ///
  /// \sa gtirb::ByteMap
  ///
  bool getDataEmpty() const;

  ///
  /// The total number of bytes in the image byte map.
  ///
  /// \sa gtirb::ByteMap
  ///
  size_t getDataSize() const;

  ///
  /// Sets byte map at the given address.
  ///
  /// The given address must be within the minimum and maximum EA.
  ///
  /// \throws std::out_of_range   Throws if the address to set data at is outside of the
  /// minimum and maximum EA.
  ///
  /// \param  ea      The address to store the data.
  /// \param  x       The data to store (honoring Endianness).
  ///
  /// \sa gtirb::ByteMap
  ///
  void setData(EA ea, uint8_t x);

  ///
  /// Sets byte map at the given address.
  ///
  /// The given address must be within the minimum and maximum EA.
  ///
  /// \throws std::out_of_range   Throws if the address to set data at is outside of the
  /// minimum and maximum EA.
  ///
  /// \param  ea      The address to store the data.
  /// \param  x       The data to store (honoring Endianness).
  ///
  /// \sa gtirb::ByteMap
  ///
  void setData(EA ea, uint16_t x);

  ///
  /// Sets byte map at the given address.
  ///
  /// The given address must be within the minimum and maximum EA.
  ///
  /// \throws std::out_of_range   Throws if the address to set data at is outside of the
  /// minimum and maximum EA.
  ///
  /// \param  ea      The address to store the data.
  /// \param  x       The data to store (honoring Endianness).
  ///
  /// \sa gtirb::ByteMap
  ///
  void setData(EA ea, uint32_t x);

  ///
  /// Sets byte map at the given address.
  ///
  /// The given address must be within the minimum and maximum EA.
  ///
  /// \throws std::out_of_range   Throws if the address to set data at is outside of the
  /// minimum and maximum EA.
  ///
  /// \param  ea      The address to store the data.
  /// \param  x       The data to store (honoring Endianness).
  ///
  /// \sa gtirb::ByteMap
  ///
  void setData(EA ea, uint64_t x);

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
  /// Get a byte of data from the byte map at the given address.
  ///
  /// \param  x       The starting address for the data.
  ///
  /// \sa gtirb::ByteMap
  ///
  uint8_t getData8(EA x) const;

  ///
  /// Get a word of data from the byte map  at the given address.
  ///
  /// \param  x       The starting address for the data.
  ///
  /// \sa gtirb::ByteMap
  ///
  uint16_t getData16(EA x) const;

  ///
  /// Get a dword of data from the byte map  at the given address.
  ///
  /// \param  x       The starting address for the data.
  ///
  /// \sa gtirb::ByteMap
  ///
  uint32_t getData32(EA x) const;

  ///
  /// Get a qword of data from the byte map  at the given address.
  ///
  /// \param  x       The starting address for the data.
  ///
  /// \sa gtirb::ByteMap
  ///
  uint64_t getData64(EA x) const;

  ///
  /// Get data from the byte map  at the given address.
  ///
  /// Use the gtirb::utilities functions (i.e. ByteArray8To16) to translate this into 16, 32,
  /// or 64-bits.
  ///
  /// \param  x       The starting address for the data.
  /// \param  bytes   The number of bytes to read.
  ///
  /// \sa gtirb::ByteMap
  ///
  std::vector<uint8_t> getData(EA x, size_t bytes) const;

  ///
  /// Get data from the byte map  at the given address until a sentinel is found or a limit is
  /// reached.
  ///
  /// Use the gtirb::utilities functions (i.e. ByteArray8To16) to translate this into 16, 32,
  /// or 64-bits.
  ///
  /// \param  x           The starting address for the data.
  /// \param  sentinel    A byte to stop the 'getData' routine for.
  /// \param  bytes         The maximum number of bytes to read.
  ///
  /// \sa gtirb::ByteMap
  ///
  std::vector<uint8_t> getDataUntil(EA x, uint8_t sentinel,
                                    size_t bytes = std::numeric_limits<size_t>::max()) const;

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
