#pragma once

#include <boost/uuid/uuid.hpp>
#include <gtirb/Export.hpp>
#include <gtirb/Any.hpp>
#include <map>
#include <string>

namespace gtirb
{
    ///
    /// \class Node
    /// \author John E. Farrier
    ///
    class GTIRB_GTIRB_EXPORT_API Node
    {
    public:
        ///
        /// Automatically assigns the Node a UUID.
        ///
        Node();

        ///
        /// Generate and assign a new Universally Unique ID (UUID).
        /// Though automatically assigned on construction, it can be manually set.
        ///
        void setUUID();

        ///
        /// Manually assign Universally Unique ID (UUID).
        /// Though automatically assigned on construction, it can be manually set.
        ///
        void setUUID(boost::uuids::uuid x);

        ///
        /// Retrieve the Node's Universally Unique ID (UUID).
        ///
        boost::uuids::uuid getUUID() const;

        ///
        /// Create or set a local property (NVP, Name Value Pair).
        ///
        void setLocalProperty(std::string name, gtirb::any value);

        ///
        /// Get a local property by name.
        /// Throws std::out_of_range if the container does not have an element with the specified key.
        ///
        gtirb::any getLocalProperty(const std::string& x) const;

        ///
        /// Remove a property.
        /// \return 	True on success.
        ///
        bool removeLocalProperty(const std::string& x);

        ///
        /// Get the total number of local properties.
        ///
        size_t getLocalPropertySize() const;

        ///
        /// Test to see if the number of local properties is zero.
        ///
        bool getLocalPropertyEmpty() const;

        ///
        /// Clear all local properties.
        ///
        void clearLocalProperties();

    private:
        boost::uuids::uuid uuid;
        std::map<std::string, gtirb::any> localProperties;
    };
}
