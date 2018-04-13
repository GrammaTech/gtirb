#pragma once

#include <boost/uuid/uuid.hpp>
#include <gtirb/LocalProperties.hpp>
#include <gtirb/Table.hpp>
#include <memory>
#include <string>

namespace gtirb
{
    ///
    /// \class Node
    /// \author John E. Farrier
    ///
    /// Copied Node objects will copy the references to the Table pointers they own.  This means that any tables owned by this node will now have at
    /// least two owners.
    ///
    class GTIRB_GTIRB_EXPORT_API Node : public LocalProperties
    {
    public:
        ///
        /// Automatically assigns the Node a UUID.
        ///
        Node();

        ///
        /// This will serve as a base class.
        ///
        virtual ~Node() = default;

        ///
        /// Get a pointer to the Node that owns this Node.
        ///
        /// This is not called "parent" because many node classes will want to use a "parent" type of relationship.
        ///
        gtirb::Node* getNodeParent() const;

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
        /// Throws std::out_of_range if the container does not have an element with the specified key.
        /// This will search up the node hierarchy until the table is found or the top node is reached.
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
        /// This does not serch up the tree.  This is the number of locally owned tables.
        ///
        size_t getTableSize() const;

        ///
        /// Test to see if the number of tables at this Node is zero.
        ///
        /// This is used to manage Table pointers.  Derived node types should expose
        /// specific functions for the tables that they own or want to provide access to.
        /// They should then return strongly typed pointers to those tables.
        ///
        /// This does not serch up the tree.  This is based on locally owned tables.
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

    private:
        gtirb::Node* nodeParent{nullptr};
        boost::uuids::uuid uuid;
        std::map<std::string, std::shared_ptr<gtirb::Table>> tables;
    };
}
