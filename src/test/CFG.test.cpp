#include <gtest/gtest.h>
#include <gtirb/CFG.hpp>
#include <gtirb/CFGSet.hpp>
#include <gtirb/RuntimeError.hpp>
#include <memory>

TEST(Unit_CFG, ctor_0)
{
    EXPECT_NO_THROW(gtirb::CFG());
}

TEST(Unit_CFG, setEA)
{
    const gtirb::EA ea{22678};

    auto node = std::make_unique<gtirb::CFG>();
    EXPECT_TRUE(node != nullptr);
    EXPECT_EQ(gtirb::EA{}, node->getEA());

    EXPECT_NO_THROW(node->setEA(ea));
    EXPECT_EQ(ea, node->getEA());
}

TEST(Unit_CFG, setProcedureName)
{
    const std::string procedureName{"Foo"};

    auto node = std::make_unique<gtirb::CFG>();
    EXPECT_TRUE(node != nullptr);
    EXPECT_EQ(std::string{}, node->getProcedureName());

    EXPECT_NO_THROW(node->setProcedureName(procedureName));
    EXPECT_EQ(procedureName, node->getProcedureName());
}
