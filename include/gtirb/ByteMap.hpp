#pragma once

#include <gtirb/EA.hpp>
#include <array>
#include <cstdint>
#include <functional>
#include <gsl/gsl>
#include <limits>
#include <map>
#include <vector>

namespace proto {
class ByteMap;
}

namespace gtirb {
///
/// \var PageSize
///
/// The size of a page of data when mapping loaded file images.
///
static constexpr uint64_t PageSize{4096};

///
/// \class ByteMap
///
/// Holds the bytes of the loaded image of the binary.  Every
/// instruction represented by a gtirb::Instruction will point
/// into this map to reference the raw bytes of the instruction.
///
class GTIRB_EXPORT_API ByteMap {
public:
  ///
  /// Tests the container for empty.
  ///
  /// Modeled after the STL API.
  ///
  /// \return 	True if the container is empty.
  ///
  bool empty() const;

  ///
  /// The total number of bytes in the image map.
  ///
  /// Modeled after the STL API.
  ///
  size_t size() const;

  ///
  /// Sets byte map at the given address.
  ///
  /// \param  ea      The address to store the data.
  /// \param  data    The data to store.
  ///
  void setData(EA ea, gsl::span<const gsl::byte> data);

  ///
  /// Get data at the given address.
  ///
  /// \param  ea       The starting address for the data.
  /// \param  bytes   The number of bytes to read.
  ///
  std::vector<uint8_t> getData(EA ea, size_t bytes) const;

  ///
  /// Get data at the given address until a sentinel is found or a limit is reached.
  ///
  /// \param  ea       	The starting address for the data.
  /// \param  sentinel   	A byte to stop the 'getData' routine for.
  /// \param  bytes	      The maximum number of bytes to read.
  ///
  std::vector<uint8_t> getDataUntil(EA ea, uint8_t sentinel,
                                    size_t bytes = std::numeric_limits<size_t>::max()) const;

  using MessageType = proto::ByteMap;
  void toProtobuf(MessageType* message) const;
  void fromProtobuf(const MessageType& message);

protected:
  ///
  /// \typedef ByteMap::Page
  ///
  /// A page of data within an image's byte map.
  ///
  typedef std::array<uint8_t, PageSize> Page;

  ///
  /// Cached lookup of a page.
  ///
  /// Gets the page if it currently exists.
  ///
  /// \return 	A pointer to the page at the address, or nullptr if it does not exist.
  ///
  const ByteMap::Page* const getPage(const EA x) const;

  ///
  /// Adds a new page to be explicitly represented in the map.
  ///
  /// \return 	A pointer to the newly created Page.
  ///
  ByteMap::Page& getOrCreatePage(const EA x);

private:
  std::map<gtirb::EA, ByteMap::Page> data;
};
} // namespace gtirb
