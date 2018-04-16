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
    /// (This class could be merged into gtirb::Node.)
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
        /// Getter's of this property should have a priori knowledge of the type so it can be
        /// appropriately cast.
        ///
        /// \param name     An arbitrary (but unique) name for a property.
        /// \param value    Storage for an arbitrary type to associate with the property name.
        ///
        void setLocalProperty(std::string name, gtirb::any value);

        ///
        /// Get a local property by name.
        /// Throws std::out_of_range if the container does not have an element with the specified
        /// key.
        ///
        /// \return Storage for an arbitrary type to associate with the property name.  Getter's of
        /// this property should have a priori knowledge of the type so it can be appropriately
        /// cast.
        ///
        gtirb::any getLocalProperty(const std::string& x) const;

        ///
        /// Remove a property.
        ///
        /// \return 	True on success.  This will fail if the property was never set.
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
        /// std::pair<std::string, gtirb::any>& foo){...});
        /// \endcode
        ///
        /// \sa gtirb::LocalProperties::endLocalProperties()
        ///
        std::map<std::string, gtirb::any>::iterator beginLocalProperties();

        ///
        /// Constant "Begin" iterator for local properties.
        /// This allows iterating over the local properties with STL algorithms.
        ///
        std::map<std::string, gtirb::any>::const_iterator beginLocalProperties() const;

        ///
        /// "End" iterator for local properties.
        /// This allows iterating over the local properties with STL algorithms.
        ///
        std::map<std::string, gtirb::any>::iterator endLocalProperties();

        ///
        /// Constant "End" iterator for local properties.
        /// This allows iterating over the local properties with STL algorithms.
        ///
        std::map<std::string, gtirb::any>::const_iterator endLocalProperties() const;

    private:
        std::map<std::string, gtirb::any> localProperties;
    };
}
