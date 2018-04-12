#pragma once

#include <gtirb/Exception.hpp>
#include <typeinfo>

namespace gtirb
{
    class GTIRB_GTIRB_EXPORT_API NodeError : public gtirb::Exception
    {
    public:
        NodeError() = default;
        NodeError(std::string file, int line);

        virtual ~NodeError() = default;

        virtual const char* what() const noexcept override;

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
