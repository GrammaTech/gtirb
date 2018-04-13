#pragma once

#include <gtirb/Export.hpp>
#include <string>
#include <utility>
#include <stdexcept>

namespace gtirb
{
    ///
    /// \class Exception
    ///
    /// The base class for all GT-IRB exceptions.
    /// Compatible with std::exception.
    ///
    class GTIRB_GTIRB_EXPORT_API Exception : public std::logic_error
    {
    public:
        Exception(const char* what = "GT-IRB Exception.");
        Exception(const std::string& what);
        Exception(const std::string& what, std::string file, int line);

        virtual ~Exception() = default;

        void setLocation(std::string file, int line);
        std::pair<std::string, int> getLocation() const;

    private:
        /// The file name which generated the exception.
        std::string file{};

        /// The line number within the file that generated the exception.
        int line{0};
    };
}
