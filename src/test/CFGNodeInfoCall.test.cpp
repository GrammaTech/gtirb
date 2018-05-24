#include <gtest/gtest.h>
#include <gtirb/CFGNode.hpp>
#include <gtirb/CFGNodeInfoCall.hpp>
#include <gtirb/RuntimeError.hpp>
#include <gtirb/Symbol.hpp>
#include <memory>

TEST(Unit_CFGNodeInfoCall, ctor_0)
{
    EXPECT_NO_THROW(gtirb::CFGNodeInfoCall());
}

TEST(Unit_CFGNodeInfoCall, setProcedureNameSymbol)
{
    auto symbol = std::make_shared<gtirb::Symbol>();
    auto node = std::make_unique<gtirb::CFGNodeInfoCall>();

    EXPECT_NO_THROW(node->setProcedureNameSymbol(symbol.get()));
}

TEST(Unit_CFGNodeInfoCall, setProcedureNameSymbol_exception)
{
    auto symbol = std::make_unique<gtirb::Symbol>();
    auto node = std::make_unique<gtirb::CFGNodeInfoCall>();

    EXPECT_THROW(node->setProcedureNameSymbol(symbol.get()), std::bad_weak_ptr);
}

TEST(Unit_CFGNodeInfoCall, setKey)
{
    const auto value = int64_t{2112};

    auto node = std::make_unique<gtirb::CFGNodeInfoCall>();
    EXPECT_EQ(int64_t{0}, node->getKey());

    EXPECT_NO_THROW(node->setKey(value));

    EXPECT_NO_THROW(node->getKey());
    EXPECT_EQ(value, node->getKey());
}

TEST(Unit_CFGNodeInfoCall, setReturnSpAdjust)
{
    const auto value = int64_t{2112};

    auto node = std::make_unique<gtirb::CFGNodeInfoCall>();
    EXPECT_EQ(int64_t{0}, node->getReturnSpAdjust());

    EXPECT_NO_THROW(node->setReturnSpAdjust(value));

    EXPECT_NO_THROW(node->getReturnSpAdjust());
    EXPECT_EQ(value, node->getReturnSpAdjust());
}

TEST(Unit_CFGNodeInfoCall, setImportTableEntryEA)
{
    const auto value = gtirb::EA{22678};

    auto node = std::make_unique<gtirb::CFGNodeInfoCall>();
    EXPECT_EQ(gtirb::EA{}, node->getImportTableEntryEA());

    EXPECT_NO_THROW(node->setImportTableEntryEA(value));

    EXPECT_NO_THROW(node->getImportTableEntryEA());
    EXPECT_EQ(value, node->getImportTableEntryEA());
}
