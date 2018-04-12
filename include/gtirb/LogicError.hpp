#pragma once

#include <gtirb/Exception.hpp>

namespace gtirb
{
    class GTIRB_GTIRB_EXPORT_API LogicError : public gtirb::Exception
    {
    public:
        LogicError() = default;
        LogicError(std::string file, int line);

        virtual ~LogicError() = default;

        virtual const char* what() const noexcept override;
    };
}
