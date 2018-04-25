#pragma once

#include <gtirb/NodeError.hpp>

namespace gtirb
{
    ///
    /// \class NodeStructureError
    /// \author John E. Farrier
    ///
    /// An exception indicating a GT-IRB node has an error specific to the node's placement in the
    /// overall structure. An example would be when a node is forced into a position in the
    /// structure which is invalid.
    ///
    class GTIRB_GTIRB_EXPORT_API NodeStructureError : public gtirb::NodeError
    {
    public:
        ///
        /// Implements a constructor from the base type.
        ///
        /// \param  what    A helpful, descriptive message to pass along with this exception.
        ///
        NodeStructureError(const char* what = "GT-IRB Node Structure Error.");

        ///
        /// Implements a constructor from the base type.
        ///
        /// \param  what    A helpful, descriptive message to pass along with this exception.
        ///
        NodeStructureError(const std::string& what);

        ///
        /// A constructor to track the file name and line number where the exception was generated.
        ///
        /// \param  what    A helpful, descriptive message to pass along with this exception.
        /// \param  file    The file name that generated this exception.
        /// \param  line    The line number within the file that generated this exception.
        ///
        NodeStructureError(const std::string& what, std::string file, int line);

        ///
        /// A defaulted trivial destructor.
        ///
        ~NodeStructureError() override = default;
    };
}
