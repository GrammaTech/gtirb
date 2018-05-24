#include <gtest/gtest.h>
#include <gtirb/CFG.hpp>
#include <gtirb/CFGNode.hpp>
#include <gtirb/CFGNodeInfoCall.hpp>
#include <gtirb/CFGSet.hpp>
#include <gtirb/EA.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Utilities.hpp>
#include <memory>

using namespace gtirb;

TEST(Unit_Utilities, ByteArray8To16)
{
    const std::vector<uint8_t> input{2, 1, 1, 2, 0, 1};
    const std::vector<uint16_t> expectedOutput{258, 513, 256};
    ASSERT_NO_THROW(gtirb::utilities::ByteArray8To16(input));

    const auto output = gtirb::utilities::ByteArray8To16(input);

    EXPECT_EQ(expectedOutput[0], output[0]);
    EXPECT_EQ(expectedOutput[1], output[1]);
    EXPECT_EQ(expectedOutput[2], output[2]);
}

TEST(Unit_Utilities, ByteArray8To16Swap)
{
    const std::vector<uint8_t> input{2, 1, 1, 2, 0, 1};
    const std::vector<uint16_t> expectedOutput{513, 258, 1};
    ASSERT_NO_THROW(gtirb::utilities::ByteArray8To16(input));

    const auto output = gtirb::utilities::ByteArray8To16(input, true);

    EXPECT_EQ(expectedOutput[0], output[0]);
    EXPECT_EQ(expectedOutput[1], output[1]);
    EXPECT_EQ(expectedOutput[2], output[2]);
}

TEST(Unit_Utilities, ByteArray8To16Error)
{
    const std::vector<uint8_t> input{2, 1, 1, 2, 0, 1, 0};
    EXPECT_THROW(gtirb::utilities::ByteArray8To16(input), std::range_error);
}

TEST(Unit_Utilities, CollectThunks_nullModule)
{
    EXPECT_NO_THROW(gtirb::utilities::CollectThunks(nullptr));
}

TEST(Unit_Utilities, CollectThunks_noCFGs)
{
    auto module = std::make_unique<gtirb::Module>();
    module->setISAID(gtirb::ISAID::X64);
    EXPECT_TRUE(module != nullptr);
    EXPECT_NO_THROW(gtirb::utilities::CollectThunks(module.get()));
}

TEST(Unit_Utilities, CollectThunks_noThunks_noISAID)
{
    auto module = std::make_unique<gtirb::Module>();
    auto cfgSet = module->getCFGSet();
    ASSERT_TRUE(cfgSet != nullptr);
    EXPECT_THROW(gtirb::utilities::CollectThunks(module.get()), std::out_of_range);

    cfgSet->createCFG(EA());
    EXPECT_THROW(gtirb::utilities::CollectThunks(module.get()), std::out_of_range);
}

TEST(Unit_Utilities, CollectThunks_noThunks_0)
{
    auto module = std::make_unique<gtirb::Module>();
    ASSERT_TRUE(module != nullptr);
    EXPECT_NO_THROW(module->setISAID(gtirb::ISAID::X64));
    EXPECT_NO_THROW(gtirb::utilities::CollectThunks(module.get()));

    auto cfgSet = module->getCFGSet();
    ASSERT_TRUE(cfgSet != nullptr);
    EXPECT_NO_THROW(gtirb::utilities::CollectThunks(module.get()));

    cfgSet->createCFG(EA());
    EXPECT_NO_THROW(gtirb::utilities::CollectThunks(module.get()));
}

TEST(Unit_Utilities, CollectThunks_noThunks_1)
{
    auto module = std::make_unique<gtirb::Module>();
    ASSERT_TRUE(module != nullptr);
    EXPECT_NO_THROW(module->setISAID(gtirb::ISAID::X64));
    EXPECT_NO_THROW(gtirb::utilities::CollectThunks(module.get()));

    auto cfgSet = module->getCFGSet();
    ASSERT_TRUE(cfgSet != nullptr);
    EXPECT_NO_THROW(gtirb::utilities::CollectThunks(module.get()));

    auto cfgPtr = cfgSet->createCFG(EA());

    EXPECT_NO_THROW(cfgPtr->setFlags(gtirb::CFG::Flags::IS_ITHUNK));
    EXPECT_NO_THROW(gtirb::utilities::CollectThunks(module.get()));

    auto thunks = gtirb::utilities::CollectThunks(module.get());
    EXPECT_TRUE(thunks.empty());
}

#if 0
TEST(Unit_Utilities, CollectThunks_simple)
{
    auto module = std::make_unique<gtirb::Module>();
    ASSERT_TRUE(module != nullptr);
    EXPECT_NO_THROW(module->setISAID(gtirb::ISAID::X64));
    EXPECT_NO_THROW(gtirb::utilities::CollectThunks(module.get()));

    auto cfgSet = module->getCFGSet();
    ASSERT_TRUE(cfgSet != nullptr);
    EXPECT_NO_THROW(gtirb::utilities::CollectThunks(module.get()));

    auto cfgPtr = cfgSet->createCFG(EA());
    ASSERT_TRUE(cfgPtr != nullptr);

    EXPECT_NO_THROW(cfgPtr->setFlags(gtirb::CFG::Flags::IS_ITHUNK));

    auto cfgNode = std::make_unique<gtirb::CFGNode>();
    ASSERT_TRUE(cfgNode != nullptr);
    auto cfgNodePtr = cfgNode.get();
    EXPECT_NO_THROW(cfgPtr->push_back(std::move(cfgNode)));

    EXPECT_NO_THROW(cfgNodePtr->setKind(gtirb::CFGNode::Kind::Call));

    auto cfgNodeInfoCall = std::make_unique<gtirb::CFGNodeInfoCall>();
    ASSERT_TRUE(cfgNodeInfoCall != nullptr);
    EXPECT_NO_THROW(cfgNodePtr->setCFGNodeInfo(std::move(cfgNodeInfoCall)));

    EXPECT_TRUE(cfgNodePtr->getCFGNodeInfo() != nullptr);

    EXPECT_NO_THROW(gtirb::utilities::CollectThunks(module.get()));
    auto thunks = gtirb::utilities::CollectThunks(module.get());

    ADD_FAILURE_AT("src/Utilities.cpp", 147)
        << "This test will fail until we finish the implementation of CFGNode and "
           "CFGNodeInfoCall.  How do we associate "
           "the symbol?  Do we set it explictly within the CFGNode or do we look it up "
           "based on EA?";

    EXPECT_FALSE(thunks.empty());
    EXPECT_EQ(size_t{1}, thunks.size());
}
#endif
