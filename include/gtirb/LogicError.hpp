#pragma once

#include <gtirb/Exception.hpp>

namespace gtirb
{
    ///
    /// \class LogicError
    /// \author John E. Farrier
    ///
    /// An exception indicating a GT-IRB internal logic error.
    ///
    class GTIRB_GTIRB_EXPORT_API LogicError : public gtirb::Exception
    {
    public:
        ///
        /// Implements a constructor from the base type.
        ///
        /// \param  what    A helpful, descriptive message to pass along with this exception.
        ///
        LogicError(const char* what = "GT-IRB Logic Error");

        ///
        /// Implements a constructor from the base type.
        ///
        /// \param  what    A helpful, descriptive message to pass along with this exception.
        ///
        LogicError(const std::string& what);

        ///
        /// A constructor to track the file name and line number where the exception was generated.
        ///
        /// \param  what    A helpful, descriptive message to pass along with this exception.
        /// \param  file    The file name that generated this exception.
        /// \param  line    The line number within the file that generated this exception.
        ///
        LogicError(const std::string& what, std::string file, int line);

        ///
        /// A defaulted trivial destructor.
        ///
        virtual ~LogicError() = default;
    };
}
