#pragma once

#include <gtirb/Any.hpp>
#include <gtirb/Export.hpp>
#include <map>
#include <string>

namespace gtirb
{
    ///
    /// \class Node
    /// \author John E. Farrier
    ///
    /// Used to compose features for the gtirb::Node.
    ///
    class GTIRB_GTIRB_EXPORT_API LocalProperties
    {
    public:
        ///
        /// This will serve as a base class.
        ///
        virtual ~LocalProperties() = default;

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

        ///
        /// "Begin" iterator for local properties.
        ///
        std::map<std::string, gtirb::any>::iterator beginLocalProperties();

        ///
        /// Constant "Begin" iterator for local properties.
        ///
        std::map<std::string, gtirb::any>::const_iterator beginLocalProperties() const;

        ///
        /// "End" iterator for local properties.
        ///
        std::map<std::string, gtirb::any>::iterator endLocalProperties();

        ///
        /// Constant "End" iterator for local properties.
        ///
        std::map<std::string, gtirb::any>::const_iterator endLocalProperties() const;

    private:
        std::map<std::string, gtirb::any> localProperties;
    };
}
