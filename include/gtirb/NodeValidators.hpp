#pragma once

#include <gtirb/Node.hpp>
#include <gtirb/NodeUtilities.hpp>

namespace gtirb
{
    ///
    /// Validates that a Node's parent is of a given type.
    ///
    /// Build a function that a Node can use in its constructor to validate its parent is of a given
    /// type.  Used to pass into gtirb::Node::addParentValidator.
    ///
    template <typename T>
    std::function<bool(const Node* const)> NodeValidatorHasParentOfType()
    {
        return [](const Node* const x) {
            const auto parent = dynamic_cast<const T* const>(x);
            return (parent != nullptr);
        };
    }

    ///
    /// Validates that a Node's parent has no other children of a given type.
    ///
    /// Build a function that a Node can use in its constructor to validate it is an only child of a
    /// given type under a parent.  Used to pass into gtirb::Node::addParentValidator.
    ///
    template <typename T>
    std::function<bool(const Node* const)> NodeValidatorHasNoSiblingsOfType()
    {
        return [](const Node* const x) {
            // We should have no siblings.
            const auto siblings = GetChildrenOfType<T>(x);
            return siblings.empty();
        };
    }
} // namespace gtirb
