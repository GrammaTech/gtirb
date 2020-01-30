//===- DataBlock.hpp --------------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2020 GrammaTech, Inc.
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
#ifndef GTIRB_DataBlock_H
#define GTIRB_DataBlock_H

#include <gtirb/Addr.hpp>
#include <gtirb/ByteInterval.hpp>
#include <gtirb/Node.hpp>
#include <cstdint>
#include <functional>
#include <optional>
#include <vector>

/// \file DataBlock.hpp
/// \brief Class gtirb::DataBlock.

namespace proto {
class DataBlock;
}
namespace gtirb {
class ByteInterval; // Forward declared for the backpointer.

///
/// \class DataBlock
///
/// \brief Represents a data object, possibly symbolic.
///
/// Does not directly store the data bytes, which are kept in the
/// \ref ImageByteMap.
///
class GTIRB_EXPORT_API DataBlock : public Node {
  DataBlock(Context& C) : Node(C, Kind::DataBlock) {}

  DataBlock(Context& C, ByteInterval* P, uint64_t S)
      : Node(C, Kind::DataBlock), Parent(P), Size(S) {}

public:
  /// \brief Create an unitialized DataBlock object.
  /// \param C        The Context in which this DataBlock will be held.
  /// \return         The newly created DataBlock.
  static DataBlock* Create(Context& C) { return C.Create<DataBlock>(C); }

  /// \brief Create a DataBlock object.
  ///
  /// \param C        The Context in which the newly-created DataBlock will be
  /// \param Size     The size of the object in bytes.
  /// \return The newly created DataBlock.
  static DataBlock* Create(Context& C, uint64_t Size) {
    return C.Create<DataBlock>(C, nullptr, Size);
  }

  /// \brief Get the \ref ByteInterval this block belongs to.
  ByteInterval* getByteInterval() { return Parent; }
  /// \brief Get the \ref ByteInterval this block belongs to.
  const ByteInterval* getByteInterval() const { return Parent; }

  /// \brief Get the size of a DataBlock.
  ///
  /// \return The size.
  ///
  uint64_t getSize() const { return Size; }

  /// \brief Get the offset from the beginning of the \ref ByteInterval this
  /// block belongs to.
  uint64_t getOffset() const;

  /// \brief Get the address of this block, if present. See \ref
  /// ByteInterval.getAddress for details on why this address may not be
  /// present.
  std::optional<Addr> getAddress() const;

  /// \brief Set the size of this block.
  ///
  /// Note that this does not automatically update any \ref ByteInterval's size,
  /// bytes, or symbolic expressions. This simply changes the extents of a block
  /// in its \ref ByteInterval.
  void setSize(uint64_t S) {
    this->mutateIndices([this, S]() { Size = S; });
  }

  /// \brief Iterator over bytes in this block.
  ///
  /// \tparam T The type of data stored in this block's byte vector. Must be
  /// a POD type that satisfies Boost's EndianReversible concept.
  template <typename T> using bytes_iterator = ByteInterval::bytes_iterator<T>;
  /// \brief Range over bytes in this block.
  ///
  /// \tparam T The type of data stored in this block's byte vector. Must be
  /// a POD type that satisfies Boost's EndianReversible concept.
  template <typename T> using bytes_range = ByteInterval::bytes_range<T>;
  /// \brief Const iterator over bytes in this block.
  ///
  /// \tparam T The type of data stored in this block's byte vector. Must be
  /// a POD type that satisfies Boost's EndianReversible concept.
  template <typename T>
  using const_bytes_iterator = ByteInterval::const_bytes_iterator<T>;
  /// \brief Const range over bytes in this block.
  ///
  /// \tparam T The type of data stored in this block's byte vector. Must be
  /// a POD type that satisfies Boost's EndianReversible concept.
  template <typename T>
  using const_bytes_range = ByteInterval::const_bytes_range<T>;

  /// \brief Get an iterator to the first byte in this block.
  ///
  /// If this block is not associated with any \ref ByteInterval, than the
  /// behavior of this function is undefined.
  ///
  /// \tparam T The type of data stored in this block's byte vector. Must be
  /// a POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the block.
  /// \param  OutputOrder The endianess you wish to read out from the block.
  template <typename T>
  bytes_iterator<T>
  bytes_begin(boost::endian::order InputOrder = boost::endian::order::native,
              boost::endian::order OutputOrder = boost::endian::order::native) {
    assert(Parent && "Block has no byte interval!");
    return Parent->bytes_begin<T>(InputOrder, OutputOrder) + getOffset();
  }

  /// \brief Get an iterator past the last byte in this block.
  ///
  /// If this block is not associated with any \ref ByteInterval, than the
  /// behavior of this function is undefined.
  ///
  /// \tparam T The type of data stored in this block's byte vector. Must be
  /// a POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the block.
  /// \param  OutputOrder The endianess you wish to read out from the block.
  template <typename T>
  bytes_iterator<T>
  bytes_end(boost::endian::order InputOrder = boost::endian::order::native,
            boost::endian::order OutputOrder = boost::endian::order::native) {
    assert(Parent && "Block has no byte interval!");
    return Parent->bytes_begin<T>(InputOrder, OutputOrder) + getOffset() + Size;
  }

  /// \brief Get a range of the bytes in this block.
  ///
  /// If this block is not associated with any \ref ByteInterval, than the
  /// behavior of this function is undefined.
  ///
  /// \tparam T The type of data stored in this block's byte vector. Must be
  /// a POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the block.
  /// \param  OutputOrder The endianess you wish to read out from the block.
  template <typename T>
  bytes_range<T>
  bytes(boost::endian::order InputOrder = boost::endian::order::native,
        boost::endian::order OutputOrder = boost::endian::order::native) {
    assert(Parent && "Block has no byte interval!");
    return bytes_range<T>(bytes_begin<T>(InputOrder, OutputOrder),
                          bytes_end<T>(InputOrder, OutputOrder));
  }

  /// \brief Get an iterator to the first byte in this block.
  ///
  /// If this block is not associated with any \ref ByteInterval, than the
  /// behavior of this function is undefined.
  ///
  /// \tparam T The type of data stored in this block's byte vector. Must be
  /// a POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the block.
  /// \param  OutputOrder The endianess you wish to read out from the block.
  template <typename T>
  const_bytes_iterator<T> bytes_begin(
      boost::endian::order InputOrder = boost::endian::order::native,
      boost::endian::order OutputOrder = boost::endian::order::native) const {
    assert(Parent && "Block has no byte interval!");
    return Parent->bytes_begin<T>(InputOrder, OutputOrder) + getOffset();
  }

  /// \brief Get an iterator past the last byte in this block.
  ///
  /// If this block is not associated with any \ref ByteInterval, than the
  /// behavior of this function is undefined.
  ///
  /// \tparam T The type of data stored in this block's byte vector. Must be
  /// a POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the block.
  /// \param  OutputOrder The endianess you wish to read out from the block.
  template <typename T>
  const_bytes_iterator<T> bytes_end(
      boost::endian::order InputOrder = boost::endian::order::native,
      boost::endian::order OutputOrder = boost::endian::order::native) const {
    assert(Parent && "Block has no byte interval!");
    return Parent->bytes_begin<T>(InputOrder, OutputOrder) + getOffset() + Size;
  }

  /// \brief Get a range of the bytes in this block.
  ///
  /// If this block is not associated with any \ref ByteInterval, than the
  /// behavior of this function is undefined.
  ///
  /// \tparam T The type of data stored in this block's byte vector. Must be
  /// a POD type that satisfies Boost's EndianReversible concept.
  ///
  /// \param  InputOrder  The endianess of the data in the block.
  /// \param  OutputOrder The endianess you wish to read out from the block.
  template <typename T>
  const_bytes_range<T>
  bytes(boost::endian::order InputOrder = boost::endian::order::native,
        boost::endian::order OutputOrder = boost::endian::order::native) const {
    assert(Parent && "Block has no byte interval!");
    return const_bytes_range<T>(bytes_begin<T>(InputOrder, OutputOrder),
                                bytes_end<T>(InputOrder, OutputOrder));
  }

  /// \brief Return the raw data underlying this block's byte vector.
  ///
  /// If this block is not associated with any \ref ByteInterval, than the
  /// behavior of this function is undefined.
  ///
  /// Much like \ref std::vector::data, this function is low-level and
  /// potentially unsafe. This pointer refers to valid memory only where an
  /// iterator would be valid to point to. Modifying the size of the byte
  /// vector may invalidate this pointer. Any endian conversions will not be
  /// performed.
  ///
  /// \tparam T The type of data stored in this block's byte vector. Must be
  /// a POD type.
  ///
  /// \retrurn A pointer to raw data.
  template <typename T> T* rawBytes() {
    assert(Parent && "Block has no byte interval!");
    return reinterpret_cast<T*>(Parent->rawBytes<uint8_t>() + getOffset());
  }

  /// \brief Return the raw data underlying this block's byte vector.
  ///
  /// If this block is not associated with any \ref ByteInterval, than the
  /// behavior of this function is undefined.
  ///
  /// Much like \ref std::vector::data, this function is low-level and
  /// potentially unsafe. This pointer refers to valid memory only where an
  /// iterator would be valid to point to. Modifying the size of the byte
  /// vector may invalidate this pointer. Any endian conversions will not be
  /// performed.
  ///
  /// \tparam T The type of data stored in this block's byte vector. Must be
  /// a POD type.
  ///
  /// \retrurn A pointer to raw data.
  template <typename T> const T* rawBytes() const {
    assert(Parent && "Block has no byte interval!");
    return reinterpret_cast<const T*>(Parent->rawBytes<uint8_t>() +
                                      getOffset());
  }

  /// @cond INTERNAL
  /// \brief The protobuf message type used for serializing DataBlock.
  using MessageType = proto::DataBlock;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a DataBlock from a protobuf message.
  ///
  /// \param C   The Context in which the deserialized DataBlock will be held.
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized DataBlock object, or null on failure.
  static DataBlock* fromProtobuf(Context& C, ByteInterval* Parent,
                                 const MessageType& Message);

  static bool classof(const Node* N) { return N->getKind() == Kind::DataBlock; }
  /// @endcond

private:
  ByteInterval* Parent{nullptr};
  uint64_t Size{0};

  void setByteInterval(ByteInterval* BI) { Parent = BI; }

  /// \brief Create a DataBlock object.
  ///
  /// \param C The Context in which the newly-created DataBlock will
  /// be held.
  /// \param Parent   The parent byte interval for the data block.
  /// \param Off      The offset at which to add the code block within its
  ///                 parent.
  /// \param Size     The size of the object in bytes.
  ///
  /// \return The newly created DataBlock.
  static DataBlock* Create(Context& C, ByteInterval* Parent, uint64_t Off,
                           uint64_t Size) {
    auto* DB = C.Create<DataBlock>(C, Parent, Size);
    if (Parent)
      Parent->Blocks.emplace(Off, DB);
    return DB;
  }

  friend class Context;
  friend class ByteInterval;
};
} // namespace gtirb

#endif // GTIRB_DataBlock_H
