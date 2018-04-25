#include <gtest/gtest.h>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/Procedure.hpp>
#include <gtirb/ProcedureSet.hpp>
#include <memory>

TEST(Unit_Procedure, ctor_0)
{
    EXPECT_NO_THROW(gtirb::Procedure());
}

TEST(Unit_Procedure, validParent)
{
    auto module = std::make_unique<gtirb::ProcedureSet>();
    auto child = std::make_unique<gtirb::Procedure>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_Procedure, validParent_noException)
{
    auto module = std::make_unique<gtirb::ProcedureSet>();
    auto child = std::make_unique<gtirb::Procedure>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_Procedure, invalidParent)
{
    auto notAParent = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::Procedure>();

    EXPECT_FALSE(child->getIsValidParent(notAParent.get()));
    EXPECT_THROW(notAParent->push_back(std::move(child)), gtirb::NodeStructureError);
}

TEST(Unit_Procedure, setEA)
{
    const gtirb::EA value{22678};

    auto node = std::make_unique<gtirb::Procedure>();
    EXPECT_NO_THROW(node->getEA());
    EXPECT_EQ(gtirb::EA{}, node->getEA());

    EXPECT_NO_THROW(node->setEA(value));
    EXPECT_EQ(value, node->getEA());
}

TEST(Unit_Procedure, getPLTEntries)
{
    auto procedure = std::make_unique<gtirb::Procedure>();
    EXPECT_NO_THROW(procedure->getPLTEntries());
}

TEST(Unit_Procedure, getPLTEntries_const)
{
    const auto procedure = std::make_unique<gtirb::Procedure>();
    EXPECT_NO_THROW(procedure->getPLTEntries());
}

TEST(Unit_Procedure, getPLTEntries_ref)
{
    const gtirb::EA entry{22678};

    auto procedure = std::make_unique<gtirb::Procedure>();

    // Scope so we can get the same reference again
    {
        auto pltEntries = procedure->getPLTEntries();
        EXPECT_TRUE(pltEntries->empty());
        pltEntries->insert(entry);

        EXPECT_FALSE(pltEntries->empty());
        EXPECT_EQ(size_t{1}, pltEntries->size());
        EXPECT_EQ(std::begin(*pltEntries), pltEntries->find(entry));
    }

    auto pltEntries = procedure->getPLTEntries();
    EXPECT_FALSE(pltEntries->empty());
    EXPECT_EQ(size_t{1}, pltEntries->size());
    EXPECT_EQ(std::begin(*pltEntries), pltEntries->find(entry));
}

TEST(Unit_Procedure, getOrCreateInstruction)
{
    const auto procedure = std::make_unique<gtirb::Procedure>();
    EXPECT_TRUE(procedure->getOrCreateInstruction() != nullptr);
    EXPECT_NO_THROW(procedure->getOrCreateInstruction());
}
