#pragma once

#include <gtirb/Exception.hpp>
#include <typeinfo>

namespace gtirb
{
    ///
    /// \class NodeError
    /// \author John E. Farrier
    ///
    /// An exception indicating a GT-IRB node has an error specific to the node's type.  An example
    /// would be when validation of its data failed at some critical point.
    ///
    class GTIRB_GTIRB_EXPORT_API NodeError : public gtirb::Exception
    {
    public:
        ///
        /// Implements a constructor from the base type.
        ///
        /// \param  what    A helpful, descriptive message to pass along with this exception.
        ///
        NodeError(const char* what = "GT-IRB Node Error.");

        ///
        /// Implements a constructor from the base type.
        ///
        /// \param  what    A helpful, descriptive message to pass along with this exception.
        ///
        NodeError(const std::string& what);

        ///
        /// A constructor to track the file name and line number where the exception was generated.
        ///
        /// \param  what    A helpful, descriptive message to pass along with this exception.
        /// \param  file    The file name that generated this exception.
        /// \param  line    The line number within the file that generated this exception.
        ///
        NodeError(const std::string& what, std::string file, int line);

        ///
        /// A defaulted trivial destructor.
        ///
        virtual ~NodeError() = default;

        ///
        /// A templated function to help set the type of node that generated the error.
        ///
        /// \code{.cpp}
        /// auto e = gtirb::NodeError("Demo Error.");
        /// e.setNodeType<gtirb::Symbol>();
        /// \endcode
        ///
        template <typename T>
        void setNodeType()
        {
            this->setNodeType(typeid(T).name());
        }

        ///
        /// Manually set the name of the node's type.
        /// Generally, typeid should be used for retrieving the name.  Because this is not set
        /// automatically, a name demangler could be used prior to calling this function.
        ///
        /// \code{.cpp}
        /// auto e = gtirb::NodeError("Demo Error.");
        /// e.setNodeType(typeid(*this).name());
        /// \endcode
        ///
        void setNodeType(std::string x);

        ///
        /// Gets the name of the type of node that generated this exception.
        ///
        /// \return The name of the type of node that generated this exception.
        ///
        std::string getNodeType() const;

    private:
        std::string nodeType;
    };
}
