#pragma once

#include <gtirb/NodeError.hpp>

namespace gtirb
{
    class GTIRB_GTIRB_EXPORT_API NodeStructureError : public gtirb::NodeError
    {
    public:
        NodeStructureError() = default;
        NodeStructureError(std::string file, int line);

        virtual ~NodeStructureError() = default;

        virtual const char* what() const noexcept override;
    };
}
