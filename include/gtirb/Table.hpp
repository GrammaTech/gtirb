#pragma once

#include <string>
#include <unordered_map>
#include <gtirb/Export.hpp>
#include <boost/serialization/export.hpp>
#include <boost/archive/polymorphic_iarchive.hpp>
#include <boost/archive/polymorphic_oarchive.hpp>

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

        ///
        /// Serialization support.
        ///
        virtual void serialize(boost::archive::polymorphic_iarchive& ar, const unsigned int version = 0) = 0;

        ///
        /// Serialization support.
        ///
        virtual void serialize(boost::archive::polymorphic_oarchive& ar, const unsigned int version = 0) const = 0;
    private:
    };
}

BOOST_SERIALIZATION_ASSUME_ABSTRACT(gtirb::Table);
