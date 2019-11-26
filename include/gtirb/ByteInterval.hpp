//===- ByteInterval.hpp -----------------------------------------*- C++ -*-===//
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

#ifndef GTIRB_BYTE_INTERVAL_H
#define GTIRB_BYTE_INTERVAL_H

#include <gtirb/CodeBlock.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtirb/Node.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/key_extractors.hpp>
#include <boost/multi_index/mem_fun.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index_container.hpp>
#include <deque>
#include <optional>
#include <variant>

/// \file ByteInterval.hpp
/// \brief Class gtirb::ByteInterval.

namespace proto {
class ByteInterval;
} // namespace proto

namespace gtirb {
using Block = std::variant<CodeBlock*, DataBlock*, SymbolicExpression*>;
uint64_t GTIRB_EXPORT_API getBlockOffset(const Block& B);

class GTIRB_EXPORT_API ByteInterval : public Node {
  // struct by_offset {};
  // struct by_pointer {};
  // using BlockSet = boost::multi_index::multi_index_container<
  //     Block,
  //     boost::multi_index::ordered_unique<
  //         boost::multi_index::tag<by_offset>,
  //         boost::multi_index::global_fun<const Block&, uint64_t,
  //                                        &getBlockOffset>>,
  //     boost::multi_index::hashed_unique<
  //         boost::multi_index::tag<by_pointer>,
  //         boost::multi_index::identity<Section*>>>;

public:
  /// \brief Create a ByteInterval object.
  ///
  /// \param C          The Context in which this interval will be held.
  ///
  /// \return The newly created ByteInterval.
  static ByteInterval* Create(Context& C, std::optional<Addr> Address,
                              uint64_t Size) {
    return C.Create<ByteInterval>(C, Address, Size);
  }

  std::optional<Addr> getAddress() const { return Address; }
  uint64_t getSize() const { return Size; }
  std::deque<uint8_t>& getBytes() { return Bytes; }
  const std::deque<uint8_t>& getBytes() const { return Bytes; }

  /// @cond INTERNAL
  /// \brief The protobuf message type used for serializing ByteInterval.
  using MessageType = proto::ByteInterval;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a ByteInterval from a protobuf message.
  ///
  /// \param C  The Context in which the deserialized ByteInterval will be held.
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized ByteInterval object, or null on failure.
  static ByteInterval* fromProtobuf(Context& C, const MessageType& Message);
  /// @endcond

private:
  ByteInterval(Context& C) : Node(C, Kind::ByteInterval) {}
  ByteInterval(Context& C, std::optional<Addr> A, uint64_t S)
      : Node(C, Kind::ByteInterval), Address(A), Size(S) {}

  std::optional<Addr> Address{};
  uint64_t Size{0};
  // BlockSet Blocks;
  std::deque<uint8_t> Bytes;

  friend class Context;
};
} // namespace gtirb

#endif // GTIRB_BYTE_INTERVAL_H
