//===- ImageByteMap.hpp -----------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2018 GrammaTech, Inc.
//
//  This code is licensed under the MIT license. See the LICENSE file in the
//  project root for license terms.
//
//  This project is sponsored by the Office of Naval Research, One Liberty
//  Center, 875 N. Randolph Street, Arlington, VA 22203 under contract #
//  N68335-17-C-0700.  The content of the information does not necessarily
//  reflect the position or policy of the Government and no official
//  endorsement should be inferred.
//
//===----------------------------------------------------------------------===//
#ifndef GTIRB_IMAGEBYTEMAP_H
#define GTIRB_IMAGEBYTEMAP_H

#include <gtirb/Addr.hpp>
#include <gtirb/ByteMap.hpp>
#include <gtirb/Node.hpp>
#include <proto/ImageByteMap.pb.h>
#include <array>
#include <boost/endian/conversion.hpp>
#include <boost/range/iterator_range.hpp>
#include <optional>
#include <set>
#include <type_traits>

/// \file ImageByteMap.hpp
/// \brief Class gtirb::ImageByteMap and related functions.

namespace gtirb {

/// \cond INTERNAL
namespace details {
template <class T> struct is_std_array : std::false_type {};
template <class T, std::size_t N>
struct is_std_array<std::array<T, N>> : std::true_type {};
} // namespace details
/// \endcond

/// \class ImageByteMap
///
/// \brief Contains the loaded raw image data for the module (binary).
class GTIRB_EXPORT_API ImageByteMap : public Node {
  ImageByteMap(Context& C) : Node(C, Kind::ImageByteMap) {}

public:
  /// \brief Create an ImageByteMap object in its default state.
  ///
  /// \param C  The Context in which this object will be held.
  ///
  /// \return The newly created object.
  static ImageByteMap* Create(Context& C) { return C.Create<ImageByteMap>(C); }

  /// \brief Set the base address of the loaded file.
  ///
  /// \param X The base address to use.
  ///
  /// \return void
  void setBaseAddress(Addr X) { BaseAddress = X; }

  /// \brief Get the base address of the loaded file.
  ///
  /// \return The address.
  Addr getBaseAddress() const { return BaseAddress; }

  /// \brief Set the entry point of the loaded file.
  ///
  /// \param X The address of the entry point to use.
  ///
  /// \return void
  void setEntryPointAddress(Addr X) { EntryPointAddress = X; }

  /// \brief Get the entry point of the loaded file.
  ///
  /// \return The address of the entry point.
  Addr getEntryPointAddress() const { return EntryPointAddress; }

  /// \brief Set the minimum and maximum effective addresses (\ref
  /// Addr) for this \ref Module.
  ///
  /// \param X A std::pair containing the (minimum, maximum) effective
  /// addresses to use.
  ///
  /// \return \c false if \p X's first is > \p X's second; \c
  /// true otherwise
  ///
  /// If \p X is an invalid pair, this method returns \c false and the
  /// min and max are not set.
  bool setAddrMinMax(std::pair<Addr, Addr> X);

  /// \brief Get the minimum and maximum effective address (Addr) for
  /// this \ref Module.
  ///
  /// \return A std::pair containing the (minimum, maximum) effective
  /// addresses.
  std::pair<Addr, Addr> getAddrMinMax() const { return EaMinMax; }

  /// \brief Set the byte order to use when getting or setting data.
  ///
  /// \param Value The byte order to use.
  ///
  /// \return void
  void setByteOrder(boost::endian::order Value) { ByteOrder = Value; }

  /// \brief Get the byte order used when getting or setting data.
  ///
  /// \return The byte order.
  boost::endian::order getByteOrder() const { return ByteOrder; }

  /// \brief Set the byte map at the specified address.
  ///
  /// \param A        The address at which to store the data. Must be greater
  ///                 than the minimum address for \c this.
  /// \param  Data    A pointer to the data to store.
  ///                 \p A + Data.size() must be less than the
  ///                 maximum address for \c this.
  ///
  /// \return  Will return \c true if the data can be assigned at the given
  /// Address, or \c false otherwise. The data passed in at the given address
  /// cannot overlap another memory region (overlays are not supported).
  ///
  /// Data is written directly without any byte order conversions.
  ///
  /// \sa gtirb::ByteMap
  /// \sa getAddrMinMax()
  template <class It, typename = std::enable_if_t<std::is_convertible_v<
                          decltype(*std::declval<It>()), std::byte>>>
  bool setData(Addr A, boost::iterator_range<It> Data) {
    if (A >= this->EaMinMax.first &&
        (A + Data.size() - 1) <= this->EaMinMax.second) {
      return this->BMap.setData(A, Data);
    }
    return false;
  }

  /// \brief Set the byte map in the specified range to a constant value.
  ///
  /// \param  A       The first address in the range. Must be greater
  ///                 than the minimum address for \c this.
  /// \param  Bytes   The number of bytes to set. (\p A + \p Bytes)
  ///                 must be less than the maximum address for \c this.
  /// \param Value    The value to set for all bytes in the range [\p A,
  ///                 \p A + \p Bytes-1].
  ///
  /// \return  Will return \c true if the data can be assigned at the given
  /// Address, or \c false otherwise. The data passed in at the given address
  /// cannot overlap another memory region (overlays are not supported).
  ///
  /// \sa gtirb::ByteMap
  /// \sa getAddrMinMax()
  bool setData(Addr A, size_t Bytes, std::byte Value);

  /// \brief Store data in the byte map at the given address,
  /// converting from native byte order.
  ///
  /// \param A        The address at which to store the data. Must be greater
  ///                 than the minimum address for \c this.
  /// \param  Data    The data to store.
  ///                 \p A + sizeof(\p Data) must be less than the
  ///                 maximum address for \c this.
  ///
  /// \tparam T       The type of the data to store. May be any
  ///                 endian-reversible POD type.
  ///
  /// \return  Will return \c true if the data can be assigned at the given
  /// Address, or \c false otherwise. The data passed in at the given address
  /// cannot overlap another memory region (overlays are not supported).
  ///
  /// \sa gtirb::ByteMap
  /// \sa getByteOrder()
  /// \sa getAddrMinMax()
  template <typename T> bool setData(Addr A, const T& Data) {
    static_assert(std::is_pod<T>::value, "T must be a POD type");
    std::array<T, 1> wrapper = {Data};
    return setData(A, wrapper);
  }

  /// \brief Store an array to the byte map at the specified address,
  /// converting elements from native byte order.
  ///
  /// \param A        The address at which to store the data. Must be greater
  ///                 than the minimum address for \c this.
  /// \param Data     The data to store. This may be a std::array of any
  ///                 endian-reversible POD type.
  ///                 \p A + sizeof(\p Data) must be less than the
  ///                 maximum address for \c this.
  ///
  ///
  /// \tparam T       The type of the array elements. Can be any
  ///                 endian-reversible POD type.
  ///
  /// \return  Will return \c true if the data can be assigned at the given
  /// Address, or \c false otherwise. The data passed in at the given address
  /// cannot overlap another memory region (overlays are not supported).
  ///
  /// \sa gtirb::ByteMap
  /// \sa getByteOrder()
  /// \sa getAddrMinMax()
  template <typename T, size_t Size>
  bool setData(Addr A, const std::array<T, Size>& Data) {
    if (this->BMap.willOverlapRegion(A, Size * sizeof(T)))
      return false;

    if (this->ByteOrder != boost::endian::order::native) {
      std::array<T, Size> reversed;
      std::transform(Data.begin(), Data.end(), reversed.begin(),
                     [this](const T& x) -> T {
                       return boost::endian::conditional_reverse(
                           x, this->ByteOrder, boost::endian::order::native);
                     });
      auto begin = reinterpret_cast<const std::byte*>(&reversed);
      auto end = reinterpret_cast<const std::byte*>(&reversed + 1);
      return this->BMap.setData(A, boost::make_iterator_range(begin, end));
    }
    auto begin = reinterpret_cast<const std::byte*>(&Data);
    auto end = reinterpret_cast<const std::byte*>(&Data + 1);
    return this->BMap.setData(A, boost::make_iterator_range(begin, end));
  }

  /// \brief Store an array of std::byte to the byte map at the specified
  /// address.
  ///
  /// \param A        The address at which to store the data. Must be greater
  ///                 than the minimum address for \c this.
  /// \param Data     \p A + sizeof(\p Data) must be less than the
  ///                 maximum address for \c this.
  ///
  ///
  /// \tparam N       The size of the array.
  ///
  /// \return  Will return \c true if the data can be assigned at the given
  /// Address, or \c false otherwise. The data passed in at the given address
  /// cannot overlap another memory region (overlays are not supported).
  ///
  /// \sa gtirb::ByteMap
  /// \sa getByteOrder()
  /// \sa getAddrMinMax()
  template <size_t Size>
  bool setData(Addr A, const std::array<std::byte, Size>& Data) {
    if (this->BMap.willOverlapRegion(A, Size * sizeof(Data)))
      return false;

    return this->BMap.setData(
        A, boost::make_iterator_range(Data.begin(), Data.end()));
  }

  /// \brief A constant range of bytes, representing a contiguous block of
  /// memory.
  using const_range = ByteMap::const_range;

  /// \brief Get data from the byte map at the specified address.
  ///
  /// \param  X       The starting address for the data.
  /// \param  Bytes   The number of bytes to read.
  ///
  /// \return An iterator range that encodes a contiguous block of memory that
  /// can be accessed directly, such as via memcpy(). Will return an empty
  /// range if the requested address or number of bytes cannot be retrieved.
  ///
  /// \sa gtirb::ByteMap
  const_range data(Addr X, size_t Bytes) const;

  /// \brief Get data from the byte map at the specified address,
  /// converting to native byte order.
  ///
  /// \param  A       The starting address for the data.
  ///
  /// \tparam T        The type of the object to be returned. May be any
  ///                  endian-reversible POD type.
  ///
  /// \return If there is data available of the appropriate size at the given
  /// address, this returns an object of type \p T, initialized from the byte
  /// map data. Otherwise, it returns nullopt.
  ///
  /// \sa gtirb::ByteMap
  template <typename T>
  std::optional<
      typename std::enable_if<!details::is_std_array<T>::value, T>::type>
  getData(Addr A) {
    static_assert(std::is_pod<T>::value, "T must be a POD type");

    T Data;
    if (this->getDataNoSwap(A, Data)) {
      boost::endian::conditional_reverse_inplace(Data, this->ByteOrder,
                                                 boost::endian::order::native);
      return Data;
    }
    return std::nullopt;
  }

  /// \brief Get an array from the byte map at the specified address,
  /// converting to native byte order.
  ///
  /// \param  A       The starting address for the data.
  ///
  /// \tparam T        The type of the value to be returned. May be a
  ///                  std::array of any endian-reversible POD type.
  ///
  /// \return If there is data available of the appropriate size at the given
  /// address, this returns an object of type \p T, initialized from the byte
  /// map data. Otherwise, it returns nullopt.
  ///
  /// \sa gtirb::ByteMap
  template <typename T>
  std::optional<
      typename std::enable_if<details::is_std_array<T>::value, T>::type>
  getData(Addr A) {
    static_assert(std::is_pod<T>::value, "T::value must be a POD type");

    T Result;
    if (getDataNoSwap(A, Result)) {
      for (auto& Elt : Result) {
        boost::endian::conditional_reverse_inplace(
            Elt, this->ByteOrder, boost::endian::order::native);
        A = A + sizeof(T);
      }
      return Result;
    }
    return std::nullopt;
  }

  /// \brief Get an array of std::byte from the byte map at the specified
  /// address.
  ///
  /// \param  A       The starting address for the data.
  ///
  /// \tparam N        The size of the array to be returned.
  ///
  /// \return If there is data available of the appropriate size at the given
  /// address, this returns an object of type \p T, initialized from the byte
  /// map data. Otherwise, it returns nullopt.
  ///
  /// \sa gtirb::ByteMap
  template <size_t Size>
  std::optional<std::array<std::byte, Size>> getData(Addr A) {
    std::array<std::byte, Size> Result;
    if (getDataNoSwap(A, Result)) {
      return Result;
    }
    return std::nullopt;
  }

  /// @cond INTERNAL
  /// \brief The protobuf message type used for serializing ImageByteMap.
  using MessageType = proto::ImageByteMap;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a ImageByteMap from a protobuf message.
  ///
  /// \param C   The Context in which the deserialized ImageByteMap will be
  /// held. \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized ImageByteMap object, or null on failure.
  static ImageByteMap* fromProtobuf(Context& C, const MessageType& Message);

  static bool classof(const Node* N) {
    return N->getKind() == Kind::ImageByteMap;
  }
  /// @endcond

private:
  template <typename T> bool getDataNoSwap(Addr A, T& Result) {
    const_range Data = this->data(A, sizeof(T));
    std::copy(Data.begin(), Data.end(), reinterpret_cast<std::byte*>(&Result));
    return Data.begin() != Data.end();
  }

  // Storage for the entire contents of the loaded image.
  gtirb::ByteMap BMap;
  std::pair<Addr, Addr> EaMinMax;
  Addr BaseAddress;
  Addr EntryPointAddress;
  boost::endian::order ByteOrder{boost::endian::order::native};

  friend class Context;
};

/// \relates ImageByteMap
/// \brief Retrieve the bytes associated with an object.
///
/// \tparam T     Any type that specifies a range of addresses via
/// getAddress() and getSize() methods (e.g. DataObject).
///
/// \param IBM    The ImageByteMap to retrieve data from.
/// \param Object The object to retrieve bytes for.
///
/// \return The bytes associated with the \p Object.
template <typename T>
ImageByteMap::const_range getBytes(const ImageByteMap& IBM, const T& Object) {
  return IBM.data(Object.getAddress(), Object.getSize());
}

} // namespace gtirb

#endif // GTIRB_IMAGEBYTEMAP_H
