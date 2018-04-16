#pragma once

#include <string>
#include <unordered_map>
#include <gtirb/Export.hpp>

namespace gtirb
{
    ///
    /// \class Table
    /// \author John E. Farrier
    ///
    /// This is just a simple base class for TableTemplate.  This allows us to store pointers to tables without any
    /// specific implementation details.
    ///
    /// \sa gtirb::TableTemplate
    ///     
    class GTIRB_GTIRB_EXPORT_API Table
    {
    public:
        ///
        /// Virtual destructor.
        ///
        /// This class can be inherited from.
        ///
        virtual ~Table() = default;

        ///
        /// Computes the total number of elements stored in the table.
        ///
        /// Mirrors the STL API.
        ///
        /// \return The total number of elements stored in the table.
        ///
        virtual size_t size() const = 0;

        ///
        /// Clears all elements from the table.
        ///
        /// Mirrors the STL API.
        ///
        virtual void clear() = 0;

    private:
    };
}
