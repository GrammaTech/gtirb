//===- CodeBlock.hpp ---------------------------------------------*- C++-*-===//
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
#ifndef GTIRB_BLOCK_H
#define GTIRB_BLOCK_H

#include <gtirb/Addr.hpp>
#include <gtirb/ByteInterval.hpp>
#include <gtirb/CFG.hpp>
#include <gtirb/CfgNode.hpp>
#include <gtirb/Export.hpp>
#include <gtirb/Node.hpp>
#include <proto/CodeBlock.pb.h>
#include <boost/range/iterator_range.hpp>
#include <cstdint>
#include <functional>
#include <optional>
#include <vector>

/// \file CodeBlock.hpp
/// \ingroup CFG_GROUP
/// \brief Classes gtirb::CodeBlock and gtirb::Offset.
/// \see CFG_GROUP

namespace proto {
class Offset;
} // namespace proto

namespace gtirb {
class ByteInterval; // Forward declared for the backpointer.

/// \class CodeBlock
///
/// \brief A basic block.
/// \see \ref CFG_GROUP
class GTIRB_EXPORT_API CodeBlock : public CfgNode {
public:
  /// \brief Create an unitialized CodeBlock object.
  /// \param C        The Context in which this CodeBlock will be held.
  /// \return         The newly created CodeBlock.
  static CodeBlock* Create(Context& C) { return C.Create<CodeBlock>(C); }

  /// \brief Create a CodeBlock object.
  ///
  /// \param C          The Context in which this block will be held.
  /// \param Size       The size of the block in bytes.
  /// \param DecodeMode The decode mode of the block.
  ///
  /// \return The newly created CodeBlock.
  static CodeBlock* Create(Context& C, uint64_t Size, uint64_t DecodeMode = 0) {
    return C.Create<CodeBlock>(C, Size, DecodeMode);
  }

  /// \brief Get the \ref ByteInterval this block belongs to.
  ByteInterval* getByteInterval() { return Parent; }
  /// \brief Get the \ref ByteInterval this block belongs to.
  const ByteInterval* getByteInterval() const { return Parent; }

  /// \brief Get the size from a \ref CodeBlock.
  ///
  /// \return The size in bytes.
  ///
  /// Use with CodeBlock::getAddress() to obtain arguments to pass to
  /// ByteMap::data() for an iterator over the contents of a \ref CodeBlock.
  uint64_t getSize() const { return Size; }

  /// \brief Get the decode mode from a \ref CodeBlock.
  ///
  /// This field is used in some ISAs where it is used to
  /// differentiate between sub-ISAs; ARM and Thumb, for example.
  ///
  /// \return The decode mode.
  uint64_t getDecodeMode() const { return DecodeMode; }

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

  /// \brief Set the decode mode of this block.
  ///
  /// This field is used in some ISAs where it is used to
  /// differentiate between sub-ISAs; ARM and Thumb, for example.
  void setDecodeMode(uint64_t DM) { DecodeMode = DM; }

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
  /// \brief The protobuf message type used for serializing CodeBlock.
  using MessageType = proto::CodeBlock;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a CodeBlock from a protobuf message.
  ///
  /// \param C  The Context in which the deserialized CodeBlock will be held.
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized CodeBlock object, or null on failure.
  static CodeBlock* fromProtobuf(Context& C, ByteInterval* Parent,
                                 const MessageType& Message);

  static bool classof(const Node* N) { return N->getKind() == Kind::CodeBlock; }
  /// @endcond

private:
  CodeBlock(Context& C) : CfgNode(C, Kind::CodeBlock) {}
  CodeBlock(Context& C, uint64_t S, uint64_t Decode)
      : CfgNode(C, Kind::CodeBlock), Size(S), DecodeMode(Decode) {}

  void setByteInterval(ByteInterval* BI) { Parent = BI; }

  ByteInterval* Parent{nullptr};
  uint64_t Size{0};
  uint64_t DecodeMode{0};

  friend class Context;
  friend class ByteInterval;
};

} // namespace gtirb

#endif // GTIRB_BLOCK_H
