#ifndef GTIRB_IMAGEBYTEMAP_H
#define GTIRB_IMAGEBYTEMAP_H

#include <gtirb/Addr.hpp>
#include <gtirb/ByteMap.hpp>
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
/// \brief Contains the loaded raw image data for the module (binary).
///
class GTIRB_EXPORT_API ImageByteMap : public Node {
public:

  /// \brief Default constructor.
  ///
  ImageByteMap() = default;


  /// \brief Copy constructor.
  /// Assigns a new UUID to the copy.
  ///
  explicit ImageByteMap(const ImageByteMap&) = default;


  /// \brief Move constructor.
  ///
  ImageByteMap(ImageByteMap&&) = default;


  /// \brief Move assignment.
  ///
  ImageByteMap& operator=(ImageByteMap&&) = default;


  /// \brief Default destructor.
  ~ImageByteMap() override = default;


  /// \brief Set the file name of the image.
  ///
  /// \param X The file name to use. DOCFIXME[path requirements?]
  ///
  /// \return void
  ///
  void setFileName(const std::string& X) { FileName = X; }


  /// \brief Get the loaded file name and path.
  ///
  /// \return The file name and path, as a std::string. DOCFIXME[what kind of path?]
  ///
  const std::string& getFileName() const { return FileName; }


  /// \brief Set the base address of the loaded file.
  ///
  /// \param X The base address to use.
  ///
  /// \return void
  ///
  void setBaseAddress(Addr X) { BaseAddress = X; }


  /// \brief Get the base address of the loaded file.
  ///
  /// \return The address.
  ///
  Addr getBaseAddress() const { return BaseAddress; }


  /// \brief Set the entry point of the loaded file.
  ///
  /// \param X The address of the entry point to use. DOCFIXME[check]
  ///
  /// \return void
  ///
  void setEntryPointAddress(Addr X) { EntryPointAddress = X; }


  /// \brief Get the entry point of the loaded file.
  ///
  /// \return The address of the entry point.
  ///
  Addr getEntryPointAddress() const { return EntryPointAddress; }


  /// \brief Set the minimum and maximum effective addresses (\ref
  /// EA) for this \ref Module.
  ///
  /// \param X A std::pair containing the (minimum, maximum) effective
  /// addresses to use.
  ///
  /// \return \c false if \p X's first is > \p X's second; \c
  /// true otherwise
  ///
  /// If \p X is an invalid pair, this method returns \c false and the
  /// min and max will be set to an invalid state
  /// (gtirb::constants::BadAddress).  The range's min and max values
  /// are inclusive.
  ///
  bool setAddrMinMax(std::pair<Addr, Addr> X);


  /// \brief Get the minimum and maximum effective address (Addr) for
  /// this \ref Module.
  ///
  /// \return A std::pair containing the (minimum, maximum) effective
  /// addresses.
  ///
  /// Check return values for gtirb::constants::BadAddress.
  ///
  std::pair<Addr, Addr> getAddrMinMax() const { return EaMinMax; }


  /// \brief DOXFIXME
  ///
  /// \param X DOCFIXME
  ///
  /// \return void
  ///
  void setRebaseDelta(int64_t X) { RebaseDelta = X; }


  /// \brief DOXFIXME
  ///
  /// \return DOCFIXME
  ///
  int64_t getRebaseDelta() const { return RebaseDelta; }


  /// \brief Mark the loaded image as having been relocated.
  ///
  /// \return void
  ///
  /// This is primarily useful for loaders that load from sources that provide
  /// already-relocated content.
  ///
  void setIsRelocated() { IsRelocated = true; }


  /// \brief Check: has the loaded image been relocated?
  ///
  /// \return \c true if the loaded image has been relocated, \c false
  /// otherwise.
  ///
  bool getIsRelocated() const { return IsRelocated; }


  /// \brief Set the byte order to use when getting or setting data.
  ///
  /// \param Value The byte order to use.
  ///
  /// \return void
  ///
  void setByteOrder(boost::endian::order Value) { ByteOrder = Value; }

  ///
  /// \brief Get the byte order used when getting or setting data.
  ///
  /// \return The byte order.
  ///
  boost::endian::order getByteOrder() const { return ByteOrder; }


  /// \brief Set the byte map at the specified address.
  ///
  /// \throws std::out_of_range   Throws if the address to set data at is
  ///                             outside of the minimum and maximum
  ///                             EA. DOCFIXME[what about the
  ///                             addresses you reach by writing
  ///                             Data?]
  ///
  /// \param  Ea      The address at which to store the data. Must be within
  ///                 the the minimum and maximum EA for \c this.
  ///
  /// \param  Data    A pointer to the data to store.
  ///                 DOCFIXME[something like "\p Ea + sizeof(\p
  ///                 Data)+1 must be within the minimum and maximum
  ///                 EA for \c this."]
  ///
  /// \return void
  ///
  /// Data is written directly without any byte order conversions.
  ///
  /// \sa gtirb::ByteMap
  /// \sa getEAMinMax()
  ///
  void setData(Addr Ea, gsl::span<const std::byte> Data);


  /// DOCFIXME[check all]
  /// \brief Set the byte map in the specified range to a constant value.
  ///
  /// \throws std::out_of_range   Throws if \p Ea is
  ///                             outside the address range for \c
  ///                             this. DOCFIXME[just Ea? or also
  ///                             (Ea+Bytes-1)?]
  ///
  /// \param  Ea      The first address in the range. Must be within the
  ///                 address range for \c this.
  ///
  /// \param  Bytes   The number of bytes to set. (\p Ea + \p Bytes - 1)
  ///                 must be within the address range for \c this.
  ///
  /// \param Value    The value to set for all bytes in the range [\p Ea,
  ///                 \p Ea + \p Bytes-1].
  ///
  /// \return void
  ///
  /// \sa gtirb::ByteMap
  /// \sa getEAMinMax()
  ///
  void setData(Addr Ea, size_t Bytes, std::byte Value);


  /// \brief Store data in the byte map at the given address,
  /// converting from native byte order.
  ///
  /// \throws std::out_of_range   Throws if \p Ea is outside the address
  ///                             range for \c this.  DOCFIXME[just
  ///                             Ea? or also Ea+sizeof(Data)-1?]
  ///
  /// \param  Ea      The address to store the data. Must be within the
  ///                 address range for \c this.
  ///
  /// \param  Data    The data to store. DOCFIXME[something
  ///                 like "\p Ea + sizeof(\p Data)-1 must be within
  ///                 the address range for \c this."]
  ///
  /// \tparam T       The type of the data to store. May be any
  ///                 endian-reversible POD type.
  ///
  /// \return void
  ///
  /// \sa gtirb::ByteMap
  /// \sa getByteOrder()
  /// \sa getEAMinMax()
  ///
  template <typename T> void setData(Addr Ea, const T& Data) {
    static_assert(std::is_pod<T>::value, "T must be a POD type");
    if (this->ByteOrder != boost::endian::order::native) {
      T reversed = boost::endian::conditional_reverse(
          Data, this->ByteOrder, boost::endian::order::native);
      this->ByteMap.setData(Ea, as_bytes(gsl::make_span(&reversed, 1)));
    } else {
      this->ByteMap.setData(Ea, as_bytes(gsl::make_span(&Data, 1)));
    }
  }


  /// \brief Store an array to the byte map at the specified address,
  /// converting elements from native byte order.
  ///
  /// \throws std::out_of_range Throws if \p Ea is outside the
  ///                           address range for \c this.
  ///                           DOCFIXME[just Ea? or also
  ///                           (Ea+sizeof(Data)-1)?]
  ///
  /// \param  Ea      The address to store the data. Must be within the
  ///                 address range for \c this.
  ///
  /// \param Data     The data to store. This may be a std::array of any
  ///                 endian-reversible POD type. DOCFIXME[something like
  ///                 "\p Ea + sizeof(\p Data)-1 must be within the
  ///                 address range for \c this."]
  ///
  /// \tparam T       The type of the array elements. Can be any
  ///                 endian-reversible POD type.
  ///
  /// \return void
  ///
  /// \sa gtirb::ByteMap
  /// \sa getByteOrder()
  /// \sa getEAMinMax()
  ///
  template <typename T, size_t Size>
  void setData(Addr Ea, const std::array<T, Size>& Data) {
    for (const auto& Elt : Data) {
      this->setData(Ea, Elt);
      Ea = Ea + sizeof(T);
    }
  }


  /// \brief Get data from the byte map at the specified address.
  ///
  /// \param  X       The starting address for the data.
  /// \param  Bytes   The number of bytes to read.
  ///
  /// \sa gtirb::ByteMap
  ///
  /// \return The bytes at address range [\p X, \p X + \p Bytes - 1]
  /// in the byte map. DOCFIXME[check]
  ///
  /// DOCFIXME[no exceptions thrown for out-of-range parameters?]
  ///
  std::vector<std::byte> getData(Addr X, size_t Bytes) const;


  /// \brief Get data from the byte map at the specified address,
  /// converting to native byte order.
  ///
  /// \param  Ea       The starting address for the data.
  ///
  /// \tparam T        The type of the object to be returned. May be any
  ///                  endian-reversible POD type.
  ///
  /// \return An object of type \p T, initialized from the byte map data.
  ///
  /// \sa gtirb::ByteMap
  ///
  /// DOCFIXME[no exceptions thrown for out-of-range Ea, Ea+sizeof(T)-1?]
  ///
  template <typename T>
  typename std::enable_if<!is_std_array<T>::value, T>::type getData(Addr Ea) {
    static_assert(std::is_pod<T>::value, "T must be a POD type");

    return boost::endian::conditional_reverse(this->getDataNoSwap<T>(Ea),
                                              this->ByteOrder,
                                              boost::endian::order::native);
  }


  /// \brief Get an array from the byte map at the specified address,
  /// converting to native byte order.
  ///
  /// \param  Ea       The starting address for the data.
  ///
  /// \tparam T        The type of the value to be returned. May be a
  ///                  std::array of any endian-reversible POD type.
  ///
  /// \return          An object of type \p T, initialized from the byte map data.
  ///
  /// \sa gtirb::ByteMap
  ///
  /// DOCFIXME[no exceptions thrown for out-of-range Ea, Ea+sizeof(T)-1?]  
  ///
  template <typename T>
  typename std::enable_if<is_std_array<T>::value, T>::type getData(Addr Ea) {
    static_assert(std::is_pod<T>::value, "T::value must be a POD type");

    auto Result = getDataNoSwap<T>(Ea);
    for (auto& Elt : Result) {
      boost::endian::conditional_reverse_inplace(Elt, this->ByteOrder,
                                                 boost::endian::order::native);
      Ea = Ea + sizeof(T);
    }
    return Result;
  }


  /// \brief DOCFIXME
  using MessageType = proto::ImageByteMap;


  /// \brief DOCFIXME
  ///
  /// \param message DOCFIXME
  ///
  /// \return void
  void toProtobuf(MessageType* message) const;


  /// \brief DOCFIXME
  ///
  /// \param message DOCFIXME
  ///
  /// \return void
  void fromProtobuf(const MessageType& message);

private:
  template <typename T> T getDataNoSwap(Addr Ea) {
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
  gtirb::ByteMap ByteMap;
  std::string FileName;
  std::pair<Addr, Addr> EaMinMax;
  Addr BaseAddress;
  Addr EntryPointAddress;
  int64_t RebaseDelta{0};
  bool IsRelocated{false};
  boost::endian::order ByteOrder{boost::endian::order::native};
};


/// \brief Get the bytes associated with an object.
///
/// \param  byteMap  DOCFIXME
///
/// \param  object   The object of interest.  
///
/// \tparam T        The type of \p object. Can be any type that specifies a
///                  range of addresses via \c getAddress() and \c
///                  getSize() methods (e.g. \ref DataObject).
///
/// \return          The bytes associated with \p object.
///
template <typename T>
std::vector<std::byte> getBytes(const ImageByteMap& byteMap, const T& object) {
  return byteMap.getData(object.getAddress(), object.getSize());
}

} // namespace gtirb

#endif // GTIRB_IMAGEBYTEMAP_H
