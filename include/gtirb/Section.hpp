//===- Section.hpp ----------------------------------------------*- C++ -*-===//
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
#ifndef GTIRB_SECTION_H
#define GTIRB_SECTION_H

#include <gtirb/Addr.hpp>
#include <gtirb/ByteInterval.hpp>
#include <gtirb/Node.hpp>
#include <boost/iterator/iterator_traits.hpp>
#include <boost/range/iterator_range.hpp>
#include <cstdint>
#include <unordered_set>

/// \file Section.hpp
/// \brief Class gtirb::Section.

namespace proto {
class Section;
}

namespace gtirb {
class Module;

/// \class Section
///
/// \brief Represents a named section of the binary.
///
/// Does not directly store the contents of the section, which are kept in
/// \ref ImageByteMap.
class GTIRB_EXPORT_API Section : public Node {
  Section(Context& C) : Node(C, Kind::Section) {}
  Section(Context& C, const std::string& N) : Node(C, Kind::Section), Name(N) {}

  using ByteIntervalSet = std::unordered_set<ByteInterval*>;

  void addByteInterval(ByteInterval* BI) { ByteIntervals.insert(BI); }
  void removeByteInterval(ByteInterval* BI) { ByteIntervals.erase(BI); }

public:
  /// \brief Create a Section object in its default state.
  ///
  /// \param C  The Context in which this object will be held.
  ///
  /// \return The newly created object.
  static Section* Create(Context& C) { return C.Create<Section>(C); }

  /// \brief Create a Section object.
  ///
  /// \param C        The Context in which this object will be held.
  /// \param Name     The name of the section.
  ///
  /// \return The newly created object.
  static Section* Create(Context& C, const std::string& Name) {
    return C.Create<Section>(C, Name);
  }

  /// \brief Equality operator overload.
  bool operator==(const Section& Other) const;

  /// \brief Inequality operator overload.
  bool operator!=(const Section& Other) const;

  /// \brief Get the name of a Section.
  ///
  /// \return The name.
  const std::string& getName() const { return Name; }

  /// \brief Iterator over \ref ByteInterval objects.
  using byte_intervals_iterator = ByteIntervalSet::iterator;
  /// \brief Range of \ref ByteInterval objects.
  using byte_intervals_range = boost::iterator_range<byte_intervals_iterator>;
  /// \brief Const iterator over \ref ByteInterval objects.
  using const_byte_intervals_iterator = ByteIntervalSet::const_iterator;
  /// \brief Const range of \ref ByteInterval objects.
  using const_byte_intervals_range =
      boost::iterator_range<const_byte_intervals_iterator>;

  /// \brief Return an iterator to the first \ref ByteInterval.
  byte_intervals_iterator byte_intervals_begin() {
    return ByteIntervals.begin();
  }
  /// \brief Return a const iterator to the first \ref ByteInterval.
  const_byte_intervals_iterator byte_intervals_begin() const {
    return ByteIntervals.begin();
  }
  /// \brief Return an iterator to the element following the last \ref
  /// ByteInterval.
  byte_intervals_iterator byte_intervals_end() { return ByteIntervals.end(); }
  /// \brief Return a const iterator to the element following the last
  /// \ref ByteInterval.
  const_byte_intervals_iterator byte_intervals_end() const {
    return ByteIntervals.end();
  }
  /// \brief Return a range of the \ref ByteInterval objects in this section.
  byte_intervals_range byte_intervals() {
    return boost::make_iterator_range(byte_intervals_begin(),
                                      byte_intervals_end());
  }
  /// \brief Return a const range of the \ref ByteInterval objects in this
  /// section.
  const_byte_intervals_range byte_intervals() const {
    return boost::make_iterator_range(byte_intervals_begin(),
                                      byte_intervals_end());
  }

  std::optional<Addr> getAddress() const {
    if (ByteIntervals.empty()) {
      return std::optional<Addr>();
    }

    Addr result{std::numeric_limits<Addr::value_type>::max()};
    for (const auto interval : ByteIntervals) {
      auto addr = interval->getAddress();
      if (addr.has_value()) {
        if (*addr < result) {
          result = *addr;
        }
      } else {
        return std::optional<Addr>();
      }
    }
    return std::optional<Addr>(result);
  }

  std::optional<uint64_t> getSize() const {
    if (ByteIntervals.empty()) {
      return std::optional<uint64_t>(0);
    }

    Addr lowAddr{std::numeric_limits<Addr::value_type>::max()};
    Addr highAddr{0};

    for (const auto interval : ByteIntervals) {
      auto addr = interval->getAddress();
      if (addr.has_value()) {
        if (*addr < lowAddr) {
          lowAddr = *addr;
        }
        Addr adjustedAddr = *addr + interval->getSize();
        if (adjustedAddr > highAddr) {
          highAddr = adjustedAddr;
        }
      } else {
        return std::optional<uint64_t>();
      }
    }

    return std::optional<uint64_t>(highAddr - lowAddr);
  }

  /// @cond INTERNAL
  /// \brief The protobuf message type used for serializing Section.
  using MessageType = proto::Section;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a Section from a protobuf message.
  ///
  /// \param C   The Context in which the deserialized Section will be held.
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized Section object, or null on failure.
  static Section* fromProtobuf(Context& C, const MessageType& Message);

  static bool classof(const Node* N) { return N->getKind() == Kind::Section; }
  /// @endcond

private:
  std::string Name;
  ByteIntervalSet ByteIntervals;

  friend class Context;
  friend class Module;
};
} // namespace gtirb

#endif // GTIRB_SECTION_H
