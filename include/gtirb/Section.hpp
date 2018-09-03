#ifndef GTIRB_SECTION_H
#define GTIRB_SECTION_H

#include <gtirb/Addr.hpp>
#include <gtirb/Node.hpp>
#include <cstdint>

namespace proto {
class Section;
}
namespace gtirb {
///
/// \class Section
///
/// \brief DOCFIXME  
  
class GTIRB_EXPORT_API Section : public Node {
public:

  /// \brief Default constructor.
  Section() = default;


  /// \brief Constructor.
  ///
  /// \param N DOCFIXME
  ///
  /// \param Address DOCFIXME
  ///
  /// \param Size DOCFIXME
  ///
  Section(std::string N, Addr Address, uint64_t Size);


  /// \brief Copy constructor.
  /// Assigns a new UUID to the copy.
  ///
  explicit Section(const Section& Other) = default;


  /// \brief Move constructor.
  ///
  Section(Section&&) = default;


  /// \brief Move assignment.
  ///
  Section& operator=(Section&&) = default;


  /// \brief Equality operator overload.
  bool operator==(const Section& Other) const;


  /// \brief Inequality operator overload.
  bool operator!=(const Section& Other) const;


  /// \brief Get the name of a Section.
  ///
  /// \return The name.
  ///
  const std::string& getName() const { return Name; }


  /// \brief Get the address of a Section
  ///
  /// \return The address.
  const Addr getAddress() const { return Address; }


  /// \brief Get the size of a Section
  ///
  /// \return The size.
  uint64_t getSize() const { return Size; }


  /// \brief DOCFIXME
  using MessageType = proto::Section;


  /// \brief DOCFIXME
  ///
  /// \param Message DOCFIXME
  ///
  /// \return void
  void toProtobuf(MessageType* Message) const;


  /// \brief DOCFIXME
  ///
  /// \param Message DOCFIXME
  ///
  /// \return void
  void fromProtobuf(const MessageType& Message);

private:
  std::string Name;
  Addr Address{0};
  uint64_t Size{0};
};
} // namespace gtirb

#endif // GTIRB_SECTION_H
