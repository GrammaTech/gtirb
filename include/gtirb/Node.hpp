#pragma once

#include <boost/uuid/uuid.hpp>
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
    using UUID = boost::uuids::uuid;

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
        /// Retrieve a node by its UUID.
        ///
        /// \return node with the given UUID, or nullptr if none exists.
        static Node* getByUUID(UUID uuid);

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
        void setUUID(UUID x);

        ///
        /// Retrieve the Node's Universally Unique ID (UUID).
        ///
        UUID getUUID() const;

        ///
        /// Create or set a local property (NVP, Name Value Pair).
        ///
        /// Getters of this property should have a priori knowledge of the type so it can be
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
        /// \return Storage for an arbitrary type to associate with the property name.  Getters of
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

    private:
        std::map<std::string, gtirb::variant> localProperties;
        UUID uuid;

        static std::map<UUID, Node*> uuidMap;
    };
} // namespace gtirb
