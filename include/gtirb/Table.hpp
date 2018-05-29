#pragma once

#include <boost/archive/polymorphic_iarchive.hpp>
#include <boost/archive/polymorphic_oarchive.hpp>
#include <boost/serialization/export.hpp>
#include <gtirb/Export.hpp>
#include <gtirb/Variant.hpp>
#include <map>
#include <string>

namespace gtirb
{
    class Node;

    ///
    /// \class Table
    ///
    /// A generic table for storing additional, client-specific data.
    ///
    /// This is a map between variants, allowing the storage of most GT-IRB
    /// types.
    ///
    class GTIRB_GTIRB_EXPORT_API Table
    {
    public:
        ~Table();

        /// Table keys can be any of these types.
        using KeyType = boost::variant<EA, uint64_t, std::string>;
        using InnerValueType = boost::variant<EA, uint64_t, std::string, std::vector<EA>,
                                              std::vector<uint64_t>, std::vector<std::string>>;
        /// Table values can also be maps, but they can only store a limited
        /// set of value types.
        using InnerMapType = std::map<KeyType, InnerValueType>;
        /// Table values can be any of these types.
        using ValueType =
            boost::variant<EA, uint64_t, std::string, InnerMapType, std::vector<InnerMapType>,
                           std::vector<EA>, std::vector<uint64_t>, std::vector<std::string>>;
        using MapType = std::map<KeyType, ValueType>;

        ///
        /// Computes the total number of elements stored in the table.
        ///
        /// Mirrors the STL API.
        ///
        /// \return The total number of elements stored in the table.
        ///
        size_t size() const;

        ///
        /// Clears all elements from the table.
        ///
        /// Mirrors the STL API.
        ///
        void clear();

        ///
        /// Serialization support.
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/);

        ///
        /// The contents of the table.
        ///
        /// \todo Make this private and provide an STL-style API.
        ///
        MapType contents;

    private:
    };
}

BOOST_CLASS_EXPORT_KEY(gtirb::Table);
