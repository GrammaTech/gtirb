#include <gtest/gtest.h>
#include <gtirb/Module.hpp>
#include <gtirb/Procedure.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <memory>

TEST(Unit_Procedure, ctor_0)
{
    EXPECT_NO_THROW(gtirb::Procedure());
}

TEST(Unit_Procedure, validParent)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::Procedure>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_Procedure, validParent_noException)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::Procedure>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TEST(Unit_Procedure, invalidParent)
{
    auto notAModule = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::Procedure>();

    EXPECT_FALSE(child->getIsValidParent(notAModule.get()));
    EXPECT_THROW(notAModule->push_back(std::move(child)), gtirb::NodeStructureError);
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
