#include <gtest/gtest.h>
#include <gtirb/NodeError.hpp>

class Foo
{
};

TEST(Unit_NodeError, setNodeType)
{
    const std::string fileName{__FILE__};
    const int lineNumber{__LINE__};

    auto e = gtirb::NodeError("Test Exception.", fileName, lineNumber);

    EXPECT_EQ(std::string{""}, e.getNodeType());

    EXPECT_NO_THROW(e.setNodeType<Foo>());
    EXPECT_EQ(std::string{typeid(Foo).name()}, e.getNodeType());
}
