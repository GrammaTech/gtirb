#pragma once

#include <gtirb/Exception.hpp>

namespace gtirb
{
    class GTIRB_GTIRB_EXPORT_API LogicError : public gtirb::Exception
    {
    public:
        LogicError(const char* what = "GT-IRB Logic Error");
		LogicError(const std::string& what);
		LogicError(const std::string& what, std::string file, int line);

        virtual ~LogicError() = default;
    };
}
