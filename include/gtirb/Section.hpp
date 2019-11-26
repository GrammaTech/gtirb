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
  Section(Context& C, const std::string& N) : Node(C, Kind::Section), Name(N) {}

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

  friend class Context;
};
} // namespace gtirb

#endif // GTIRB_SECTION_H
