#pragma once

#include <boost/serialization/base_object.hpp>
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
#include <gtirb/Table.hpp>
#include <gtirb/Variant.hpp>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace gtirb
{
    ///
    /// \class Node
    /// \author John E. Farrier
    ///
    /// Copied Node objects will copy the references to the Table pointers they own.  This means
    /// that any tables owned by this node will now have at least two owners.
    ///
    class GTIRB_GTIRB_EXPORT_API Node : public std::enable_shared_from_this<gtirb::Node>
    {
    public:
        ///
        /// A custom STL-compatible iterator for Node children.
        ///
        class iterator
        {
        public:
            typedef iterator self_type;
            typedef gtirb::Node* const value_type;
            typedef gtirb::Node* const reference;
            typedef gtirb::Node* const pointer;
            typedef std::forward_iterator_tag iterator_category;
            typedef int difference_type;

            iterator(std::vector<std::shared_ptr<Node>>::iterator x) : it(x)
            {
            }

            self_type operator++()
            {
                self_type i = *this;
                this->it++;
                return i;
            }

            self_type operator++(int)
            {
                this->it++;
                return *this;
            }

            reference operator*()
            {
                return this->it->get();
            }

            pointer operator->()
            {
                return this->it->get();
            }

            bool operator==(const self_type& rhs) const
            {
                return this->it == rhs.it;
            }

            bool operator!=(const self_type& rhs) const
            {
                return this->it != rhs.it;
            }

        private:
            std::vector<std::shared_ptr<Node>>::iterator it;
        };

        ///
        /// A custom STL-compatible const_iterator for Node children.
        ///
        class const_iterator
        {
        public:
            typedef const_iterator self_type;
            typedef gtirb::Node* const value_type;
            typedef gtirb::Node* const reference;
            typedef gtirb::Node* const pointer;
            typedef std::forward_iterator_tag iterator_category;
            typedef int difference_type;

            const_iterator(std::vector<std::shared_ptr<Node>>::const_iterator x) : it(x)
            {
            }

            self_type operator++()
            {
                self_type i = *this;
                this->it++;
                return i;
            }

            self_type operator++(int)
            {
                this->it++;
                return *this;
            }

            const reference operator*()
            {
                return this->it->get();
            }

            const pointer operator->()
            {
                return this->it->get();
            }

            bool operator==(const self_type& rhs) const
            {
                return this->it == rhs.it;
            }

            bool operator!=(const self_type& rhs) const
            {
                return this->it != rhs.it;
            }

        private:
            std::vector<std::shared_ptr<Node>>::const_iterator it;
        };

        ///
        /// Automatically assigns the Node a UUID.
        ///
        Node();

        ///
        /// This will serve as a base class for other nodes.
        /// The destructor is trivial and defaulted.
        ///
        virtual ~Node() = default;

        ///
        /// Get a pointer to the Node that owns this Node.
        ///
        /// This is not called "parent" because many node classes will want to use a "parent" type
        /// of relationship.
        ///
        gtirb::Node* getNodeParent() const;

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
        /// Checks to see if 'x' can be a parent of this node.
        ///
        bool getIsValidParent(const Node* const x) const;

        ///
        /// Adds a child node.
        ///
        /// API is modeled after the STL.  Executes functions added via
        /// Node::addPushBackValidator().  Will not add the Node if the Node's validator returns
        /// false.
        ///
        /// Throws gtirb::NodeStructureError.
        ///
        void push_back(std::unique_ptr<gtirb::Node>&& x);

        ///
        /// Determines if there are any child nodes.
        ///
        /// API is modeled after the STL.
        ///
        /// \return     True of there are not any child nodes.
        ///
        bool empty() const;

        ///
        /// Returns the number of elements in the container.
        /// The number of child nodes.  API is modeled after the STL.  Constant complexity.
        ///
        /// \return     Zero for an empty structure, or the number of child nodes.
        ///
        size_t size() const;

        ///
        /// Clear all children from this node.
        ///
        /// API is modeled after the STL.
        ///
        void clear();

        ///
        /// Access specified element with bounds checking.
        ///
        /// \param      x   The position of the element to return.
        /// \return     A pointer to the Node child at the given index.
        ///
        Node* at(size_t x);

        ///
        /// Access specified element with bounds checking.
        ///
        /// \param      x   The position of the element to return.
        /// \return     A const pointer to the Node child at the given index.
        ///
        const Node* const at(size_t x) const;

        ///
        /// Returns a Node::iterator to the first child Node.
        ///
        /// If the container is empty, the returned iterator will be equal to end().
        ///
        /// \return     An iterator to the first child Node.
        ///
        Node::iterator begin();

        ///
        /// Returns a Node::iterator to the element following the last element of the container.
        ///
        /// This element acts as a placeholder; attempting to access it results in undefined
        /// behavior.
        ///
        /// \return     An iterator to the element following the last element of the container.
        ///
        Node::iterator end();

        ///
        /// Returns a Node::const_iterator to the first child Node.
        ///
        /// If the container is empty, the returned iterator will be equal to end().
        ///
        /// \return     An iterator to the first child Node.
        ///
        Node::const_iterator begin() const;

        ///
        /// Returns a Node::const_iterator to the element following the last element of the
        /// container.
        ///
        /// This element acts as a placeholder; attempting to access it results in undefined
        /// behavior.
        ///
        /// \return     An iterator to the element following the last element of the container.
        ///
        Node::const_iterator end() const;

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
        template <class Archive>
        void serialize(Archive& ar, const unsigned int)
        {
            ar & this->localProperties;
            ar & this->tables;
            ar & this->children;
            ar & this->uuid;
            ar & this->nodeParent;
        }

    protected:
        // ----------------------------------------------------------------------------------------
        // Table Properties

        ///
        /// Locally ownership of a table.
        /// The table can be populated from anywhere.
        ///
        /// This is used to manage Table pointers.  Derived node types should expose
        /// specific functions for the tables that they own or want to provide access to.
        /// They should then return strongly typed pointers to those tables.
        ///
        /// \param name     The name to assign to the table so it can be found later.
        /// \param x        An owning pointer to the table itself.
        ///
        void addTable(std::string name, std::unique_ptr<gtirb::Table>&& x);

        ///
        /// A table by name.
        ///
        /// This is used to manage Table pointers.  Derived node types should expose
        /// specific functions for the tables that they own or want to provide access to.
        /// They should then return strongly typed pointers to those tables.
        ///
        /// Throws std::out_of_range if the container does not have an element with the specified
        /// key. This will search up the node hierarchy until the table is found or the top node is
        /// reached.
        ///
        /// \param  x   The name of the table to search for.
        /// \return     A pointer to the table if found, or nullptr.
        ///
        gtirb::Table* const getTable(const std::string& x) const;

        ///
        /// Remove a table.
        ///
        /// This is used to manage Table pointers.  Derived node types should expose
        /// specific functions for the tables that they own or want to provide access to.
        /// They should then return strongly typed pointers to those tables.
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
        /// This is used to manage Table pointers.  Derived node types should expose
        /// specific functions for the tables that they own or want to provide access to.
        /// They should then return strongly typed pointers to those tables.
        ///
        /// This does not search up the tree.  This is the number of locally owned tables.
        ///
        /// \return     The total number of tables this node owns.
        ///
        size_t getTableSize() const;

        ///
        /// Test to see if the number of tables at this Node is zero.
        ///
        /// This is used to manage Table pointers.  Derived node types should expose
        /// specific functions for the tables that they own or want to provide access to.
        /// They should then return strongly typed pointers to those tables.
        ///
        /// This does not search up the tree.  This is based on locally owned tables.
        ///
        /// \return     True if this node owns at least one table.
        ///
        bool getTablesEmpty() const;

        ///
        /// Clear all locally owned tables.
        ///
        /// This is used to manage Table pointers.  Derived node types should expose
        /// specific functions for the tables that they own or want to provide access to.
        /// They should then return strongly typed pointers to those tables.
        ///
        void clearTables();

        ///
        /// Add a function to validate a parent relationship.
        ///
        /// Derived types should call this in their constructors.
        ///
        void addParentValidator(std::function<bool(const Node* const)> x);

    private:
        std::vector<std::function<bool(const Node* const)>> parentValidators;

        std::map<std::string, gtirb::variant> localProperties;
        std::map<std::string, std::shared_ptr<gtirb::Table>> tables;
        std::vector<std::shared_ptr<Node>> children;
        std::string uuid;
        gtirb::Node* nodeParent{nullptr};
    };
} // namespace gtirb

BOOST_CLASS_EXPORT_KEY(gtirb::Node);
