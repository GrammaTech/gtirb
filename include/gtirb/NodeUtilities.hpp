#pragma once

#include <gtirb/Node.hpp>
#include <gtirb/NodeStructureError.hpp>

namespace gtirb
{
    ///
    /// A free function to build a vector of Node children of a specific type.
    ///
    /// \param  x   A pointer to the node to get the children of.
    /// \return     A vector of all the children of the input node which are of the templated type.
    ///
    template <typename T>
    std::vector<T*> GetChildrenOfType(const Node* const x)
    {
        static_assert(std::is_base_of<gtirb::Node, T>::value,
                      "T must be a descendant of gtirb::Node.");

        std::vector<T*> children;

        std::for_each(std::begin(*x), std::end(*x), [&children](Node* const child) {
            auto correctType = dynamic_cast<T*>(child);
            if(correctType != nullptr)
            {
                children.push_back(correctType);
            }
        });

        return children;
    }

    ///
    /// Get or create a child node based on type.
    ///
    /// This function is useful for types that cannot have same-type siblings under the same parent
    /// node.
    ///
    /// \param      x   A pointer to the node to get or create the child under.
    /// \return     A pointer of type T to an existing or newly created child.
    ///
    template <typename T>
    T* GetOrCreateChildOfType(Node* const x)
    {
        const auto children = GetChildrenOfType<T>(x);

        if(children.empty() == false)
        {
            if(children.size() == 1)
            {
                return children[0];
            }
            else
            {
                throw gtirb::NodeStructureError("Multiple \"" + std::string(typeid(T).name())
                                                + "\" children were found under one Node of type \""
                                                + std::string(typeid(*x).name()) + "\".");
            }
        }

        auto type = std::make_unique<T>();
        auto typePtr = type.get();

        x->push_back(std::move(type));
        return typePtr;
    }
} // namespace gtirb
