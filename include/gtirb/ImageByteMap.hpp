#pragma once

#include <gtirb/ByteMap.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/Node.hpp>
#include <proto/ImageByteMap.pb.h>
#include <array>
#include <boost/endian/conversion.hpp>
#include <gsl/gsl>
#include <set>
#include <type_traits>

namespace gtirb {

template <class T> struct is_std_array : std::false_type {};
template <class T, std::size_t N>
struct is_std_array<std::array<T, N>> : std::true_type {};

///
/// \class ImageByteMap
///
/// Contains the loaded raw image data for the module (binary).
///
class GTIRB_EXPORT_API ImageByteMap : public Node {
  ImageByteMap() = default;

public:
  static ImageByteMap *Create(Context &C) { return new (C) ImageByteMap; }

  ///
  /// \return     Sets the file name of the image.
  ///
  void setFileName(std::string X);

  ///
  /// \return     The loaded file name and path.
  ///
  std::string getFileName() const;

  ///
  /// Sets the base address of loaded file.
  ///
  void setBaseAddress(EA X);

  ///
  /// Gets the base addrress of loaded file.
  ///
  EA getBaseAddress() const;

  ///
  /// Sets the entry point of loaded file.
  ///
  void setEntryPointAddress(EA X);

  ///
  /// Gets the entry point of loaded file.
  ///
  EA getEntryPointAddress() const;

  ///
  /// If an invalid pair is passed in, the min and max will be set to an invalid
  /// state (gtirb::constants::BadAddress).  The range's min and max values are
  /// inclusive.
  ///
  /// \param      x   The minimum and maximum effective address (EA) for this
  /// Module. \return     False if the pair's first is > the pair's second.
  ///
  bool setEAMinMax(std::pair<gtirb::EA, gtirb::EA> X);

  ///
  /// Gets the minimum and maximum effective address (EA) for this Module.
  ///
  /// Check return values for gtirb::constants::BadAddress.
  ///
  /// \return     The minimum and maximum effective address (EA) for this
  /// Module.
  ///
  std::pair<gtirb::EA, gtirb::EA> getEAMinMax() const;

  ///
  ///
  ///
  void setRebaseDelta(int64_t X);

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
  /// Set the byte order to use when getting or setting data.
  ///
  boost::endian::order getByteOrder() const;

  ///
  /// Get the byte order used when getting or setting data.
  ///
  void setByteOrder(boost::endian::order Value);

  ///
  /// Sets byte map at the given address. Data is written directly without
  /// any byte order conversions.
  ///
  /// The given address must be within the minimum and maximum EA.
  ///
  /// \throws std::out_of_range   Throws if the address to set data at is
  /// outside of the minimum and maximum EA.
  ///
  /// \param  ea      The address to store the data.
  /// \param  data    A pointer to the data to store.
  ///
  /// \sa gtirb::ByteMap
  ///
  void setData(EA Ea, gsl::span<const std::byte> Data);

  ///
  /// Sets byte map in the given range to a constant value.
  ///
  /// The given address must be within the minimum and maximum EA.
  ///
  /// \throws std::out_of_range   Throws if the address to set data at is
  /// outside of the minimum and maximum EA.
  ///
  /// \param  ea      The address to store the data.
  /// \param  value   The value for all bytes in the range.
  ///
  /// \sa gtirb::ByteMap
  ///
  void setData(EA Ea, size_t Bytes, std::byte Value);

  ///
  /// Stores data in the byte map at the given address, converting from native
  /// byte order.
  ///
  /// The given address must be within the minimum and maximum EA.
  ///
  /// \throws std::out_of_range   Throws if the address to set data at is
  /// outside of the minimum and maximum EA.
  ///
  /// \param  Ea      The address to store the data.
  /// \param  Data    The data to store. This may be any endian-reversible POD
  /// type.
  ///
  /// \sa gtirb::ByteMap
  ///
  template <typename T> void setData(EA Ea, const T& Data) {
    static_assert(std::is_pod<T>::value, "T must be a POD type");
    if (this->ByteOrder != boost::endian::order::native) {
      T reversed = boost::endian::conditional_reverse(
          Data, this->ByteOrder, boost::endian::order::native);
      this->BMap.setData(Ea, as_bytes(gsl::make_span(&reversed, 1)));
    } else {
      this->BMap.setData(Ea, as_bytes(gsl::make_span(&Data, 1)));
    }
  }

  ///
  /// Stores an array to the byte map at the given address, converting
  /// elements from native byte order.
  ///
  /// \throws std::out_of_range   Throws if the address to set data at is
  /// outside of the minimum and maximum EA.
  ///
  /// \param  Ea      The address to store the data.

  /// \param Data     The data to store. This may be a std::array of any
  ///                 endian-reversible POD type.
  ///
  /// \sa gtirb::ByteMap
  ///
  template <typename T, size_t Size>
  void setData(EA Ea, const std::array<T, Size>& Data) {
    for (const auto& Elt : Data) {
      this->setData(Ea, Elt);
      Ea += sizeof(T);
    }
  }

  ///
  /// Get data from the byte map at the given address.
  ///
  /// \param  x       The starting address for the data.
  /// \param  bytes   The number of bytes to read.
  ///
  /// \sa gtirb::ByteMap
  ///
  std::vector<std::byte> getData(EA X, size_t Bytes) const;

  ///
  /// Get data from the byte map at the given address, converting to native
  /// byte order.
  ///
  /// Returns an object of type T, initialized from the byte map data.
  /// T may be any endian-reversible POD type.
  ///
  /// \param  Ea       The starting address for the data.
  ///
  /// \sa gtirb::ByteMap
  ///
  template <typename T>
  typename std::enable_if<!is_std_array<T>::value, T>::type getData(EA Ea) {
    static_assert(std::is_pod<T>::value, "T must be a POD type");

    return boost::endian::conditional_reverse(this->getDataNoSwap<T>(Ea),
                                              this->ByteOrder,
                                              boost::endian::order::native);
  }

  ///
  /// Get an array from the byte map at the given address, converting to
  /// native byte order.
  ///
  /// Returns an object of type T, initialized from the byte map data.
  /// T may be a std::array of any endian-reversible POD type.
  ///
  /// \param  ea       The starting address for the data.
  ///
  /// \sa gtirb::ByteMap
  ///
  template <typename T>
  typename std::enable_if<is_std_array<T>::value, T>::type getData(EA Ea) {
    static_assert(std::is_pod<T>::value, "T::value must be a POD type");

    auto Result = getDataNoSwap<T>(Ea);
    for (auto& Elt : Result) {
      boost::endian::conditional_reverse_inplace(Elt, this->ByteOrder,
                                                 boost::endian::order::native);
      Ea += sizeof(T);
    }
    return Result;
  }

  using MessageType = proto::ImageByteMap;
  void toProtobuf(MessageType* message) const;
  static ImageByteMap *fromProtobuf(Context &C, const MessageType& message);

private:
  template <typename T> T getDataNoSwap(EA Ea) {
    T Result;
    auto destSpan = as_writeable_bytes(gsl::make_span(&Result, 1));
    // Assign this to a variable so it isn't destroyed before we copy
    // from it (because gsl::span is non-owning).
    auto data = this->getData(Ea, destSpan.size_bytes());
    auto srcSpan = as_bytes(gsl::make_span(data));
    assert(srcSpan.size() == destSpan.size());

    std::copy(srcSpan.begin(), srcSpan.end(), destSpan.begin());
    return Result;
  }

  // Storage for the entire contents of the loaded image.
  gtirb::ByteMap BMap;
  std::string FileName;
  std::pair<gtirb::EA, gtirb::EA> EaMinMax{};
  EA BaseAddress{};
  EA EntryPointAddress{};
  int64_t RebaseDelta{0};
  bool IsRelocated{false};
  boost::endian::order ByteOrder{boost::endian::order::native};
};

///
/// Retrieve the bytes associated with an object.
///
/// Object can be any type which specifies a range of addresses via
/// getAddress() and getSize() methods (e.g. Data).
///
template <typename T>
std::vector<std::byte> getBytes(const ImageByteMap& byteMap, const T& object) {
  return byteMap.getData(object.getAddress(), object.getSize());
}

} // namespace gtirb
