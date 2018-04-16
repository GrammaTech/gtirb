#include <gtest/gtest.h>
#include <gtirb/Module.hpp>
#include <gtirb/ModuleCore.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <memory>

TEST(Unit_ModuleCore, ctor_0)
{
    EXPECT_NO_THROW(gtirb::ModuleCore());
}

TEST(Unit_ModuleCore, validParent)
{
    auto module = std::make_unique<gtirb::Module>();
    auto ModuleCore = std::make_unique<gtirb::ModuleCore>();
    EXPECT_TRUE(ModuleCore->getIsValidParent(module.get()));
    EXPECT_TRUE(module->push_back(std::move(ModuleCore)));
}

TEST(Unit_ModuleCore, validParent_noException)
{
    auto module = std::make_unique<gtirb::Module>();
    auto ModuleCore = std::make_unique<gtirb::ModuleCore>();
    EXPECT_TRUE(ModuleCore->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(ModuleCore)));
}

TEST(Unit_ModuleCore, invalidParent)
{
    auto notAModule = std::make_unique<gtirb::Node>();
    auto ModuleCore = std::make_unique<gtirb::ModuleCore>();

    EXPECT_FALSE(ModuleCore->getIsValidParent(notAModule.get()));
    EXPECT_THROW(notAModule->push_back(std::move(ModuleCore)), gtirb::NodeStructureError);
}
