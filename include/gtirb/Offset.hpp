
//===- Offset.hpp ------------------------------------------------*- C++-*-===//
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
#ifndef GTIRB_OFFSET_H
#define GTIRB_OFFSET_H

#include <gtirb/Context.hpp>
#include <gtirb/Export.hpp>
#include <proto/Offset.pb.h>
#include <boost/functional/hash.hpp>
#include <cstdint>
#include <functional>

namespace gtirb {

/// \class Offset
///
/// \brief Describes a location inside a block or data object.
struct GTIRB_EXPORT_API Offset {
  /// \brief The UUID of the block or data object.
  UUID ElementId;

  /// \brief The displacement from the start of the block or data object, in
  /// bytes.
  uint64_t Displacement{0};

  /// \brief Constructor using a ElemId uuid and a Displacement.
  Offset(const UUID& ElemId, uint64_t Disp)
      : ElementId(ElemId), Displacement(Disp) {}

  /// \brief Default constructor.
  Offset() = default;

  /// @cond INTERNAL
  /// \brief The protobuf message type used for serializing Offset.
  using MessageType = proto::Offset;

  /// \brief Serialize into a protobuf message.
  ///
  /// \param[out] Message   Serialize into this message.
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;

  /// \brief Construct a Offset from a protobuf message.
  ///
  /// \param C  The Context in which the deserialized Offset will be
  ///           held.
  /// \param Message  The protobuf message from which to deserialize.
  ///
  /// \return The deserialized Offset object, or null on failure.
  void fromProtobuf(Context& C, const MessageType& Message);
  /// @endcond

  /// \brief Equality operator for \ref Offset.
  // Note: boost::uuid is not constexpr.
  friend bool operator==(const Offset& LHS, const Offset& RHS) noexcept {
    return LHS.ElementId == RHS.ElementId &&
           LHS.Displacement == RHS.Displacement;
  }

  /// \brief Inequality operator for \ref Offset.
  friend bool operator!=(const Offset& LHS, const Offset& RHS) noexcept {
    return !operator==(LHS, RHS);
  }

  /// \brief Less-than operator for \ref Offset.
  friend constexpr bool operator<(const Offset& LHS,
                                  const Offset& RHS) noexcept {
    return std::tie(LHS.ElementId, LHS.Displacement) <
           std::tie(RHS.ElementId, RHS.Displacement);
  }

  /// \brief Greater-than operator for \ref Offset.
  friend constexpr bool operator>(const Offset& LHS,
                                  const Offset& RHS) noexcept {
    return operator<(RHS, LHS);
  }

  /// \brief Less-than-or-equal operator for \ref Offset.
  friend constexpr bool operator<=(const Offset& LHS,
                                   const Offset& RHS) noexcept {
    return !operator<(RHS, LHS);
  }

  /// \brief Greater-than-or-equal operator for \ref Offset.
  friend constexpr bool operator>=(const Offset& LHS,
                                   const Offset& RHS) noexcept {
    return !operator<(LHS, RHS);
  }
};

} // namespace gtirb

namespace std {

/// \brief Hash operation for \ref Offset.
template <> struct hash<gtirb::Offset> {
  size_t operator()(const gtirb::Offset& X) const {
    std::size_t Seed = 0;
    boost::hash_combine(Seed, X.ElementId);
    boost::hash_combine(Seed, X.Displacement);
    return Seed;
  }
};

} // namespace std

#endif // GTIRB_OFFSET_H
