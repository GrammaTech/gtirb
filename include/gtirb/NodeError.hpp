#pragma once

#include <gtirb/Exception.hpp>
#include <typeinfo>

namespace gtirb
{
    class GTIRB_GTIRB_EXPORT_API NodeError : public gtirb::Exception
    {
    public:
        NodeError(const char* what = "GT-IRB Node Error.");
        NodeError(const std::string& what);
        NodeError(const std::string& what, std::string file, int line);

        virtual ~NodeError() = default;

        template <typename T>
        void setNodeType()
        {
            this->setNodeType(typeid(T).name());
        }

        void setNodeType(std::string x);
        std::string getNodeType() const;

    private:
        std::string nodeType;
    };
}
