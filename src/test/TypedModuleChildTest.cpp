#include <gtest/gtest.h>
#include <boost/archive/polymorphic_binary_iarchive.hpp>
#include <boost/archive/polymorphic_binary_oarchive.hpp>
#include <boost/archive/polymorphic_text_iarchive.hpp>
#include <boost/archive/polymorphic_text_oarchive.hpp>
#include <boost/filesystem.hpp>
#include <boost/serialization/shared_ptr.hpp>
#include <boost/serialization/shared_ptr_helper.hpp>
#include <gtirb/AddrRanges.hpp>
#include <gtirb/CFGSet.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Instruction.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/ModuleAux.hpp>
#include <gtirb/ModuleCore.hpp>
#include <gtirb/ModuleSectionBase.hpp>
#include <gtirb/ModuleSummary.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/NodeUtilities.hpp>
#include <gtirb/ProcedureSet.hpp>
#include <gtirb/RegionSet.hpp>
#include <gtirb/SymbolSet.hpp>
#include <memory>

using testing::Types;

typedef Types<gtirb::ModuleCore,    //
              gtirb::ModuleAux,     //
              gtirb::ModuleSummary, //
              gtirb::AddrRanges,    //
              gtirb::SymbolSet,     //
              gtirb::RegionSet,     //
              gtirb::ImageByteMap,  //
              gtirb::ProcedureSet,  //
              gtirb::CFGSet>
    TypeImplementations;

// ----------------------------------------------------------------------------
// Typed test fixture.

template <class T>
class TypedModuleChildTest : public testing::Test
{
protected:
    TypedModuleChildTest() = default;
    virtual ~TypedModuleChildTest() = default;
};

TYPED_TEST_CASE_P(TypedModuleChildTest);

// ----------------------------------------------------------------------------
// Tests to run on all types.

TYPED_TEST_P(TypedModuleChildTest, ctor_0)
{
    EXPECT_NO_THROW(TypeParam{});
}

TYPED_TEST_P(TypedModuleChildTest, validParent)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<TypeParam>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TYPED_TEST_P(TypedModuleChildTest, validParent_noException)
{
    auto module = std::make_unique<gtirb::Module>();
    auto child = std::make_unique<TypeParam>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));
}

TYPED_TEST_P(TypedModuleChildTest, invalidParent)
{
    auto notAParent = std::make_unique<gtirb::Node>();
    auto child = std::make_unique<TypeParam>();

    EXPECT_FALSE(child->getIsValidParent(notAParent.get()));
    EXPECT_THROW(notAParent->push_back(std::move(child)), gtirb::NodeStructureError);
}

TYPED_TEST_P(TypedModuleChildTest, alreadyAdded)
{
    auto module = std::make_unique<gtirb::Module>();

    auto child = std::make_unique<TypeParam>();
    EXPECT_TRUE(child->getIsValidParent(module.get()));
    EXPECT_NO_THROW(module->push_back(std::move(child)));

    auto childAgain = std::make_unique<TypeParam>();
    EXPECT_FALSE(childAgain->getIsValidParent(module.get()));
    EXPECT_THROW(module->push_back(std::move(childAgain)), gtirb::NodeStructureError);
}

REGISTER_TYPED_TEST_CASE_P(TypedModuleChildTest,    //
                           ctor_0,                  //
                           validParent,             //
                           validParent_noException, //
                           invalidParent,           //
                           alreadyAdded             //
                           );

INSTANTIATE_TYPED_TEST_CASE_P(Unit_ModuleChildren,  // Instance name
                              TypedModuleChildTest, // Test case name
                              TypeImplementations); // Type list