#pragma once

#include <gtirb/Exception.hpp>

namespace gtirb
{
    class GTIRB_GTIRB_EXPORT_API RuntimeError : public gtirb::Exception
    {
    public:
        RuntimeError(const char* what = "GT-IRB Runtime Error.");
        RuntimeError(const std::string& what);
        RuntimeError(const std::string& what, std::string file, int line);

        virtual ~RuntimeError() = default;
    };
}
