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
#include <gtirb/Node.hpp>
#include <cstdint>

/// \file Section.hpp
/// \brief Class gtirb::Section.

namespace proto {
class Section;
}

namespace gtirb {

/// \class Section
///
/// \brief Represents a named section of the binary.
///
/// Does not directly store the contents of the section, which are kept in
/// \ref ImageByteMap.
class GTIRB_EXPORT_API Section : public Node {
  Section(Context& C) : Node(C, Kind::Section) {}
  Section(Context& C, const std::string& N, Addr A, uint64_t S)
      : Node(C, Kind::Section), Name(N), Address(A), Size(S) {}

public:
  /// \brief Create a Section object in its default state.
  ///
  /// \param C  The Context in which this object will be held.
  ///
  /// \return The newly created object.
  static Section* Create(Context& C) { return new (C) Section(C); }

  /// \brief Create a Section object.
  ///
  /// \param C        The Context in which this object will be held.
  /// \param Name     The name of the section.
  /// \param Address  The initial address of the section.
  /// \param Size     The size of the section, in bytes.
  ///
  /// \return The newly created object.
  static Section* Create(Context& C, const std::string& Name, Addr Address,
                         uint64_t Size) {
    return new (C) Section(C, Name, Address, Size);
  }

  /// \brief Equality operator overload.
  bool operator==(const Section& Other) const;

  /// \brief Inequality operator overload.
  bool operator!=(const Section& Other) const;

  /// \brief Get the name of a Section.
  ///
  /// \return The name.
  const std::string& getName() const { return Name; }

  /// \brief Get the address of a Section
  ///
  /// \return The address.
  const Addr getAddress() const { return Address; }

  /// \brief Get the size of a Section
  ///
  /// \return The size.
  uint64_t getSize() const { return Size; }

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

  /// \cond INTERNAL
  static bool classof(const Node* N) { return N->getKind() == Kind::Section; }
  /// \endcond

private:
  std::string Name;
  Addr Address{0};
  uint64_t Size{0};
};
} // namespace gtirb

#endif // GTIRB_SECTION_H
