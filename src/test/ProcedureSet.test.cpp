#include <gtest/gtest.h>
#include <gtirb/Module.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/ProcedureSet.hpp>
#include <memory>

TEST(Unit_ProcedureSet, ctor_0)
{
    EXPECT_NO_THROW(gtirb::ProcedureSet());
}

TEST(Unit_ProcedureSet, validParent)
{
    auto parent = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<gtirb::ProcedureSet>();
    EXPECT_TRUE(child->getIsValidParent(parent.get()));
    EXPECT_NO_THROW(parent->push_back(std::move(child)));
}

TEST(Unit_ProcedureSet, invalidParent)
{
    auto notAParent = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<gtirb::ProcedureSet>();

    EXPECT_FALSE(child->getIsValidParent(notAParent.get()));
    EXPECT_THROW(notAParent->push_back(std::move(child)), gtirb::NodeStructureError);
}

TEST(Unit_ProcedureSet, noSiblings)
{
    auto parent = std::make_unique<gtirb::Module>();

    // Only the first child is valid.
    {
        auto child = std::make_unique<gtirb::ProcedureSet>();
        EXPECT_TRUE(child->getIsValidParent(parent.get()));
        EXPECT_NO_THROW(parent->push_back(std::move(child)));
    }

    {
        auto child = std::make_unique<gtirb::ProcedureSet>();
        EXPECT_FALSE(child->getIsValidParent(parent.get()));
        EXPECT_THROW(parent->push_back(std::move(child)), gtirb::NodeStructureError);
    }
}

TEST(Unit_ProcedureSet, getProcedure)
{
    gtirb::EA ea{22678};
    auto node = std::make_unique<gtirb::ProcedureSet>();
    EXPECT_NO_THROW(node->getProcedure(ea));
    EXPECT_TRUE(node->empty());
    EXPECT_EQ(size_t{0}, node->size());

    auto procedure = node->getProcedure(ea);
    EXPECT_TRUE(procedure == nullptr);
    EXPECT_TRUE(node->empty());
    EXPECT_EQ(size_t{0}, node->size());

    procedure = node->getOrCreateProcedure(ea);
    EXPECT_TRUE(procedure != nullptr);
    EXPECT_FALSE(node->empty());
    EXPECT_EQ(size_t{1}, node->size());

    procedure = node->getProcedure(ea);
    EXPECT_TRUE(procedure != nullptr);
    EXPECT_FALSE(node->empty());
    EXPECT_EQ(size_t{1}, node->size());

    // Make sure we don't create it again.
    procedure = node->getOrCreateProcedure(ea);
    EXPECT_TRUE(procedure != nullptr);
    EXPECT_FALSE(node->empty());
    EXPECT_EQ(size_t{1}, node->size());
}

TEST(Unit_ProcedureSet, getProcedure_invalid)
{
    gtirb::EA ea{};
    auto node = std::make_unique<gtirb::ProcedureSet>();
    EXPECT_NO_THROW(node->getProcedure(ea));
}
