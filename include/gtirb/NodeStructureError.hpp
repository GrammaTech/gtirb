#pragma once

#include <gtirb/NodeError.hpp>

namespace gtirb
{
    class GTIRB_GTIRB_EXPORT_API NodeStructureError : public gtirb::NodeError
    {
    public:
        NodeStructureError(const char* what = "GT-IRB Node Structure Error.");
        NodeStructureError(const std::string& what);
        NodeStructureError(const std::string& what, std::string file, int line);

        virtual ~NodeStructureError() = default;
    };
}
