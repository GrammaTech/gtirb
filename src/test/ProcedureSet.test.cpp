#include <gtest/gtest.h>
#include <gtirb/Module.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/ProcedureSet.hpp>
#include <memory>

TEST(Unit_ProcedureSet, ctor_0)
{
    EXPECT_NO_THROW(gtirb::ProcedureSet());
}

TEST(Unit_ProcedureSet, getProcedure)
{
    gtirb::EA ea{22678};
    auto node = std::make_unique<gtirb::ProcedureSet>();
    EXPECT_NO_THROW(node->getProcedure(ea));

    auto procedure = node->getProcedure(ea);
    EXPECT_TRUE(procedure == nullptr);

    procedure = node->createProcedure(ea);
    EXPECT_TRUE(procedure != nullptr);

    procedure = node->getProcedure(ea);
    EXPECT_TRUE(procedure != nullptr);
}

TEST(Unit_ProcedureSet, getProcedure_invalid)
{
    gtirb::EA ea{};
    auto node = std::make_unique<gtirb::ProcedureSet>();
    EXPECT_NO_THROW(node->getProcedure(ea));
}
