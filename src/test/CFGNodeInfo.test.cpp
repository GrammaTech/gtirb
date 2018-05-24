#include <gtest/gtest.h>
#include <gtirb/CFGNode.hpp>
#include <gtirb/CFGNodeInfo.hpp>
#include <gtirb/RuntimeError.hpp>
#include <gtirb/Symbol.hpp>
#include <memory>

TEST(Unit_CFGNodeInfo, ctor_0)
{
    EXPECT_NO_THROW(gtirb::CFGNodeInfo());
}

TEST(Unit_CFGNodeInfo, setProcedureNameSymbol)
{
    auto symbol = std::make_shared<gtirb::Symbol>();
    auto node = std::make_unique<gtirb::CFGNodeInfo>();

    EXPECT_NO_THROW(node->setProcedureNameSymbol(symbol.get()));
}

TEST(Unit_CFGNodeInfo, setProcedureNameSymbol_exception)
{
    auto symbol = std::make_unique<gtirb::Symbol>();
    auto node = std::make_unique<gtirb::CFGNodeInfo>();

    EXPECT_THROW(node->setProcedureNameSymbol(symbol.get()), std::bad_weak_ptr);
}
