#pragma once

#include <boost/serialization/base_object.hpp>
#include <boost/serialization/export.hpp>
#include <boost/serialization/list.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/set.hpp>
#include <boost/serialization/shared_ptr.hpp>
#include <boost/serialization/string.hpp>
#include <boost/serialization/unique_ptr.hpp>
#include <boost/serialization/unordered_map.hpp>
#include <boost/serialization/unordered_set.hpp>
#include <boost/serialization/variant.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/serialization/version.hpp>
#include <functional>
#include <gsl/gsl>
#include <gtirb/Export.hpp>
#include <gtirb/Variant.hpp>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace gtirb
{
    class Table;

    ///
    /// \class Node
    /// \author John E. Farrier
    ///
    /// Copied Node objects will copy the references to the Table pointers they own.  This means
    /// that any tables owned by this node will now have at least two owners.
    ///
    class GTIRB_GTIRB_EXPORT_API Node
    {
    public:
        ///
        /// Automatically assigns the Node a UUID.
        ///
        Node();

        ///
        /// This will serve as a base class for other nodes.
        /// The destructor is trivial and defaulted.
        ///
        virtual ~Node();

        ///
        /// Generate and assign a new Universally Unique ID (UUID).
        ///
        /// Though automatically assigned on construction, it can be manually set.
        ///
        void setUUID();

        ///
        /// Manually assign Universally Unique ID (UUID).
        ///
        /// Though automatically assigned on construction, it can be manually set.
        ///
        void setUUID(std::string x);

        ///
        /// Retrieve the Node's Universally Unique ID (UUID).
        ///
        std::string getUUID() const;

        ///
        /// Create or set a local property (NVP, Name Value Pair).
        ///
        /// Getter's of this property should have a priori knowledge of the type so it can be
        /// appropriately cast.
        ///
        /// \param name     An arbitrary (but unique) name for a property.
        /// \param value    Storage for an arbitrary type to associate with the property name.
        ///
        void setLocalProperty(std::string name, gtirb::variant value);

        ///
        /// Get a local property by name.
        /// Throws std::out_of_range if the container does not have an element with the specified
        /// key.
        ///
        /// \return Storage for an arbitrary type to associate with the property name.  Getter's of
        /// this property should have a priori knowledge of the type so it can be appropriately
        /// cast.
        ///
        gtirb::variant getLocalProperty(const std::string& x) const;

        ///
        /// Remove a property.
        ///
        /// \return     True on success.  This will fail if the property was never set.
        ///
        bool removeLocalProperty(const std::string& x);

        ///
        /// Get the total number of local properties.
        ///
        /// \return     The total number of locally stored properties.
        ///
        size_t getLocalPropertySize() const;

        ///
        /// Test to see if the number of local properties is zero.
        ///
        /// \return     True if there are no local properties.  False if at least one property has
        /// been set.
        ///
        bool getLocalPropertyEmpty() const;

        ///
        /// Clear all local properties.
        /// After calling this function, a call to 'getLocalPropertiesEmpty()' should return 'true'.
        ///
        void clearLocalProperties();

        ///
        /// "Begin" iterator for local properties.
        /// This allows iterating over the local properties with STL algorithms.
        ///
        /// \code{.cpp}
        /// std::for_each(x->beginLocalProperties(), x->endLocalProperties(), [](const
        /// std::pair<std::string, gtirb::variant>& foo){...});
        /// \endcode
        ///
        /// \sa gtirb::LocalProperties::endLocalProperties()
        ///
        std::map<std::string, gtirb::variant>::iterator beginLocalProperties();

        ///
        /// Constant "Begin" iterator for local properties.
        /// This allows iterating over the local properties with STL algorithms.
        ///
        std::map<std::string, gtirb::variant>::const_iterator beginLocalProperties() const;

        ///
        /// "End" iterator for local properties.
        /// This allows iterating over the local properties with STL algorithms.
        ///
        std::map<std::string, gtirb::variant>::iterator endLocalProperties();

        ///
        /// Constant "End" iterator for local properties.
        /// This allows iterating over the local properties with STL algorithms.
        ///
        std::map<std::string, gtirb::variant>::const_iterator endLocalProperties() const;

        ///
        /// Serialization support.
        ///
        /// This is built for boost::serialization.
        ///
        /// \note
        /// The Cereal library was also considered, but does not handle std::shared_ptr as nicely.
        /// (May 2017)
        ///
        template <class Archive>
        void serialize(Archive& ar, const unsigned int /*version*/);

        // ----------------------------------------------------------------------------------------
        // Table Properties

        ///
        /// Add a new table, transferring ownership.
        /// The table can be populated from anywhere.
        ///
        /// \param name     The name to assign to the table so it can be found later.
        /// \param x        An owning pointer to the table itself.
        /// \return a non-owning pointer to the added table.
        ///
        Table* addTable(std::string name, std::unique_ptr<gtirb::Table>&& x);

        ///
        /// Get a table by name.
        ///
        /// \param  x   The name of the table to search for.
        /// \return     A pointer to the table if found, or nullptr.
        ///
        gtirb::Table* const getTable(const std::string& x) const;

        ///
        /// Remove a table by name.
        ///
        /// This will invalidate any pointers that may have been held externally.
        ///
        /// \param  x   The name of the table to search for.
        /// \return     True on success.
        ///
        bool removeTable(const std::string& x);

        ///
        /// Get the total number of tables at this Node.
        ///
        /// \return     The total number of tables this node owns.
        ///
        size_t getTableSize() const;

        ///
        /// Test to see if the number of tables at this Node is zero.
        ///
        /// \return     True if this node does not own any tables.
        ///
        bool getTablesEmpty() const;

        ///
        /// Clear all locally owned tables.
        ///
        void clearTables();

    private:
        std::map<std::string, gtirb::variant> localProperties;
        std::map<std::string, std::shared_ptr<gtirb::Table>> tables;
        std::string uuid;
    };
} // namespace gtirb

BOOST_CLASS_EXPORT_KEY(gtirb::Node);
