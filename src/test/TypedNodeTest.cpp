#include <gtest/gtest.h>
#include <boost/archive/polymorphic_binary_iarchive.hpp>
#include <boost/archive/polymorphic_binary_oarchive.hpp>
#include <boost/archive/polymorphic_text_iarchive.hpp>
#include <boost/archive/polymorphic_text_oarchive.hpp>
#include <boost/filesystem.hpp>
#include <boost/serialization/shared_ptr.hpp>
#include <boost/serialization/shared_ptr_helper.hpp>
#include <gtirb/AddrRanges.hpp>
#include <gtirb/CFG.hpp>
#include <gtirb/CFGNode.hpp>
#include <gtirb/CFGNodeInfo.hpp>
#include <gtirb/CFGNodeInfoActualIn.hpp>
#include <gtirb/CFGNodeInfoCall.hpp>
#include <gtirb/CFGNodeInfoDeclares.hpp>
#include <gtirb/CFGNodeInfoEntry.hpp>
#include <gtirb/CFGNodeInfoFormalIn.hpp>
#include <gtirb/CFGSet.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/Instruction.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/ModuleAux.hpp>
#include <gtirb/ModuleCore.hpp>
#include <gtirb/ModuleSectionBase.hpp>
#include <gtirb/ModuleSummary.hpp>
#include <gtirb/NodeStructureError.hpp>
#include <gtirb/NodeUtilities.hpp>
#include <gtirb/Procedure.hpp>
#include <gtirb/ProcedureSet.hpp>
#include <gtirb/Region.hpp>
#include <gtirb/RegionSet.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolSet.hpp>
#include <memory>

using testing::Types;

typedef Types<gtirb::AddrRanges,          //
              gtirb::CFG,                 //
              gtirb::CFGSet,              //
              gtirb::CFGNode,             //
              gtirb::CFGNodeInfo,         //
              gtirb::CFGNodeInfoActualIn, //
              gtirb::CFGNodeInfoCall,     //
              gtirb::CFGNodeInfoDeclares, //
              gtirb::CFGNodeInfoEntry,    //
              gtirb::CFGNodeInfoFormalIn, //
              gtirb::ImageByteMap,        //
              gtirb::Instruction,         //
              gtirb::IR,                  //
              gtirb::Module,              //
              gtirb::ModuleAux,           //
              gtirb::ModuleCore,          //
              gtirb::ModuleSectionBase,   //
              gtirb::ModuleSummary,       //
              gtirb::Node,                //
              gtirb::Procedure,           //
              gtirb::Region,              //
              gtirb::RegionSet,           //
              gtirb::Symbol,              //
              gtirb::ProcedureSet,        //
              gtirb::SymbolSet            //
              >
    TypeImplementations;

// ----------------------------------------------------------------------------
// Typed test fixture.

template <class T>
class TypedNodeTest : public testing::Test
{
protected:
    TypedNodeTest() = default;
    virtual ~TypedNodeTest() = default;
};

TYPED_TEST_CASE_P(TypedNodeTest);

// ----------------------------------------------------------------------------
// Tests to run on all types.

TYPED_TEST_P(TypedNodeTest, ctor_0)
{
    EXPECT_NO_THROW(TypeParam{});
}

TYPED_TEST_P(TypedNodeTest, uniqueUuids)
{
    std::vector<std::string> uuids;

    // Create a bunch of UUID's, then make sure we don't have any duplicates.

    for(size_t i = 0; i < 64; ++i)
    {
        const auto n = TypeParam{};
        uuids.push_back(n.getUUID());
    }

    std::sort(std::begin(uuids), std::end(uuids));
    const auto end = std::unique(std::begin(uuids), std::end(uuids));

    EXPECT_EQ(std::end(uuids), end) << "Duplicate UUID's were generated.";
}

TYPED_TEST_P(TypedNodeTest, setLocalProperty)
{
    auto node = TypeParam{};

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp{"Foo", "Bar"};
    EXPECT_NO_THROW(node.setLocalProperty(nvp.first, nvp.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());
}

TYPED_TEST_P(TypedNodeTest, setLocalPropertyReset)
{
    auto node = TypeParam{};

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp1{"Foo", "Bar"};
    const std::pair<std::string, std::string> nvp2{"Foo", "Bah"};

    EXPECT_NO_THROW(node.setLocalProperty(nvp1.first, nvp1.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());

    EXPECT_NO_THROW(node.setLocalProperty(nvp2.first, nvp2.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());
}

TYPED_TEST_P(TypedNodeTest, setLocalProperties)
{
    auto node = TypeParam{};

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp1{"Foo", "Bar"};
    const std::pair<std::string, std::string> nvp2{"Bar", "Foo"};

    EXPECT_NO_THROW(node.setLocalProperty(nvp1.first, nvp1.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());

    EXPECT_NO_THROW(node.setLocalProperty(nvp2.first, nvp2.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(2), node.getLocalPropertySize());
}

TYPED_TEST_P(TypedNodeTest, removeLocalProperty)
{
    auto node = TypeParam{};

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp1{"Foo", "Bar"};
    const std::pair<std::string, std::string> nvp2{"Bar", "Foo"};

    EXPECT_NO_THROW(node.setLocalProperty(nvp1.first, nvp1.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());

    EXPECT_NO_THROW(node.setLocalProperty(nvp2.first, nvp2.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(2), node.getLocalPropertySize());

    EXPECT_TRUE(node.removeLocalProperty("Foo"));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());

    EXPECT_TRUE(node.removeLocalProperty("Bar"));
    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());
}

TYPED_TEST_P(TypedNodeTest, clearLocalProperties)
{
    auto node = TypeParam{};

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());

    const std::pair<std::string, std::string> nvp1{"Foo", "Bar"};
    const std::pair<std::string, std::string> nvp2{"Bar", "Foo"};

    EXPECT_NO_THROW(node.setLocalProperty(nvp1.first, nvp1.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(1), node.getLocalPropertySize());

    EXPECT_NO_THROW(node.setLocalProperty(nvp2.first, nvp2.second));
    EXPECT_FALSE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(2), node.getLocalPropertySize());

    EXPECT_NO_THROW(node.clearLocalProperties());

    EXPECT_TRUE(node.getLocalPropertyEmpty());
    EXPECT_EQ(size_t(0), node.getLocalPropertySize());
}

TYPED_TEST_P(TypedNodeTest, push_back)
{
    auto node = TypeParam{};

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());

    EXPECT_NO_THROW(node.push_back(std::make_unique<gtirb::Node>()));

    EXPECT_FALSE(node.empty());
    EXPECT_EQ(size_t(1), node.size());
}

TYPED_TEST_P(TypedNodeTest, size)
{
    const int totalChildren = 64;
    auto node = TypeParam{};

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());

    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_NO_THROW(node.push_back(std::make_unique<gtirb::Node>()));
        EXPECT_FALSE(node.empty());
        EXPECT_EQ(size_t(i + 1), node.size());
    }
}

TYPED_TEST_P(TypedNodeTest, clear)
{
    const int totalChildren = 64;
    auto node = TypeParam{};

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());

    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_NO_THROW(node.push_back(std::make_unique<gtirb::Node>()));
        EXPECT_FALSE(node.empty());
        EXPECT_EQ(size_t(i + 1), node.size());
    }

    EXPECT_NO_THROW(node.clear());

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());
}

TYPED_TEST_P(TypedNodeTest, iterator)
{
    const int totalChildren = 8;

    auto node = TypeParam{};

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());

    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_NO_THROW(node.push_back(std::make_unique<gtirb::Node>()));
        EXPECT_FALSE(node.empty());
        EXPECT_EQ(size_t(i + 1), node.size());
    }

    // We should now be able to use our begin iterator "totalChildren" times.
    auto nodeIterator = std::begin(node);
    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_TRUE(nodeIterator != std::end(node));

        EXPECT_EQ(node.at(i)->getUUID(), nodeIterator->getUUID());
        ++nodeIterator;

        EXPECT_NE(nodeIterator, std::begin(node));
    }

    EXPECT_NE(nodeIterator, std::begin(node));
    EXPECT_EQ(nodeIterator, std::end(node));
}

TYPED_TEST_P(TypedNodeTest, const_iterator)
{
    const int totalChildren = 8;

    auto node = TypeParam{};

    EXPECT_TRUE(node.empty());
    EXPECT_EQ(size_t(0), node.size());

    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_NO_THROW(node.push_back(std::make_unique<gtirb::Node>()));
        EXPECT_FALSE(node.empty());
        EXPECT_EQ(size_t(i + 1), node.size());
    }

    // Make a const node by copy construction.
    const auto constNode{node};

    // We should now be able to use our begin iterator "totalChildren" times.
    auto nodeIterator = std::begin(constNode);
    for(int i = 0; i < totalChildren; i++)
    {
        EXPECT_TRUE(nodeIterator != std::end(constNode));

        EXPECT_EQ(constNode.at(i)->getUUID(), nodeIterator->getUUID());
        ++nodeIterator;

        EXPECT_NE(nodeIterator, std::begin(constNode));
    }

    EXPECT_NE(nodeIterator, std::begin(constNode));
    EXPECT_EQ(nodeIterator, std::end(constNode));
}

TYPED_TEST_P(TypedNodeTest, shared_from_this)
{
    auto node = std::make_shared<TypeParam>();
    auto nodePtr = node.get();

    ASSERT_NO_THROW(nodePtr->shared_from_this())
        << "The node did not inherit from std::enable_shared_from_this<Node>.";

    auto nodeShared = nodePtr->shared_from_this();
    EXPECT_TRUE(nodeShared != nullptr);
}

TYPED_TEST_P(TypedNodeTest, GetChildrenOfType)
{
    class Foo : public gtirb::Node
    {
    };

    class Bar : public gtirb::Node
    {
    };

    const int fooChildren = 3;
    const int barChildren = 5;
    const int typeChildren = 8;

    TypeParam node;

    for(int i = 0; i < fooChildren; i++)
    {
        node.push_back(std::make_unique<Foo>());
    }

    for(int i = 0; i < barChildren; i++)
    {
        node.push_back(std::make_unique<Bar>());
    }

    const auto childrenOfTypeFoo = gtirb::GetChildrenOfType<Foo>(&node);
    EXPECT_EQ(size_t(fooChildren), childrenOfTypeFoo.size());

    const auto childrenOfTypeBar = gtirb::GetChildrenOfType<Bar>(&node);
    EXPECT_EQ(size_t(barChildren), childrenOfTypeBar.size());

    const auto allChildren = gtirb::GetChildrenOfType<gtirb::Node>(&node);
    EXPECT_EQ(size_t(fooChildren + barChildren), allChildren.size());
}

TYPED_TEST_P(TypedNodeTest, serialize)
{
    const auto tempPath =
        boost::filesystem::temp_directory_path() / boost::filesystem::unique_path();
    const std::string tempPathString = tempPath.string();

    TypeParam original;
    original.setLocalProperty("Name", std::string("Value"));

    // Scope objects so they are destroyed
    {
        EXPECT_EQ(size_t{1}, original.getLocalPropertySize());
        EXPECT_EQ(std::string{"Value"}, boost::get<std::string>(original.getLocalProperty("Name")));

        // Serialize Out.
        std::ofstream ofs{tempPathString.c_str()};
        boost::archive::polymorphic_text_oarchive oa{ofs};
        EXPECT_TRUE(ofs.is_open());

        EXPECT_NO_THROW(oa << original);

        EXPECT_NO_THROW(ofs.close());
        EXPECT_FALSE(ofs.is_open());
    }

    // Read it back in and re-test
    {
        TypeParam serialized;

        // Serialize In.
        std::ifstream ifs{tempPathString.c_str()};
        boost::archive::polymorphic_text_iarchive ia{ifs};

        EXPECT_NO_THROW(ia >> serialized);

        EXPECT_NO_THROW(ifs.close());

        ASSERT_EQ(size_t{1}, serialized.getLocalPropertySize());
        EXPECT_EQ(std::string{"Value"},
                  boost::get<std::string>(serialized.getLocalProperty("Name")));
        EXPECT_EQ(original.getUUID(), serialized.getUUID());
        EXPECT_EQ(original.size(), serialized.size());
        EXPECT_EQ(sizeof(original), sizeof(serialized));
    }
}

TYPED_TEST_P(TypedNodeTest, serializeBinary)
{
    const auto tempPath =
        boost::filesystem::temp_directory_path() / boost::filesystem::unique_path();
    const std::string tempPathString = tempPath.string();

    TypeParam original;
    original.setLocalProperty("Name", std::string("Value"));

    // Scope objects so they are destroyed
    {
        EXPECT_EQ(size_t{1}, original.getLocalPropertySize());
        EXPECT_EQ(std::string{"Value"}, boost::get<std::string>(original.getLocalProperty("Name")));

        // Serialize Out.
        std::ofstream ofs{tempPathString.c_str(), std::ofstream::binary};
        boost::archive::polymorphic_binary_oarchive oa{ofs};
        EXPECT_TRUE(ofs.is_open());

        EXPECT_NO_THROW(oa << original);

        EXPECT_NO_THROW(ofs.close());
        EXPECT_FALSE(ofs.is_open());
    }

    // Read it back in and re-test
    {
        TypeParam serialized;

        // Serialize In.
        std::ifstream ifs{tempPathString.c_str(), std::ifstream::binary};
        boost::archive::polymorphic_binary_iarchive ia{ifs};

        EXPECT_NO_THROW(ia >> serialized);

        EXPECT_NO_THROW(ifs.close());

        ASSERT_EQ(size_t{1}, serialized.getLocalPropertySize());
        EXPECT_EQ(std::string{"Value"},
                  boost::get<std::string>(serialized.getLocalProperty("Name")));
        EXPECT_EQ(original.getUUID(), serialized.getUUID());
        EXPECT_EQ(original.size(), serialized.size());
        EXPECT_EQ(sizeof(original), sizeof(serialized));
    }
}

TYPED_TEST_P(TypedNodeTest, serializeFromSharedPtr)
{
    const auto tempPath =
        boost::filesystem::temp_directory_path() / boost::filesystem::unique_path();
    const std::string tempPathString = tempPath.string();

    auto original = std::make_shared<TypeParam>();
    original->setLocalProperty("Name", std::string("Value"));

    // Scope objects so they are destroyed
    {
        EXPECT_EQ(size_t{1}, original->getLocalPropertySize());
        EXPECT_EQ(std::string{"Value"},
                  boost::get<std::string>(original->getLocalProperty("Name")));

        // Serialize Out.
        std::ofstream ofs{tempPathString.c_str()};
        boost::archive::polymorphic_text_oarchive oa{ofs};
        EXPECT_TRUE(ofs.is_open());

        EXPECT_NO_THROW(oa << original);

        EXPECT_NO_THROW(ofs.close());
        EXPECT_FALSE(ofs.is_open());
    }

    // Read it back in and re-test
    {
        auto serialized = std::make_shared<TypeParam>();

        // Serialize In.
        std::ifstream ifs{tempPathString.c_str()};
        boost::archive::polymorphic_text_iarchive ia{ifs};

        EXPECT_NO_THROW(ia >> serialized);

        EXPECT_NO_THROW(ifs.close());

        ASSERT_EQ(size_t{1}, serialized->getLocalPropertySize());
        EXPECT_EQ(std::string{"Value"},
                  boost::get<std::string>(serialized->getLocalProperty("Name")));

        EXPECT_EQ(original->getUUID(), serialized->getUUID());
        EXPECT_EQ(original->size(), serialized->size());
        EXPECT_EQ(sizeof(original), sizeof(serialized));
        EXPECT_EQ(sizeof(*original), sizeof(*serialized));
        EXPECT_EQ(typeid(*original), typeid(*serialized));
    }
}

TYPED_TEST_P(TypedNodeTest, serializeFromSharedPtrWChildren)
{
    const auto tempPath =
        boost::filesystem::temp_directory_path() / boost::filesystem::unique_path();
    const std::string tempPathString = tempPath.string();

    auto original = std::make_shared<TypeParam>();
    original->setLocalProperty("Name", std::string("Value"));

    auto foo = std::make_unique<gtirb::Node>();
    foo->setLocalProperty("Name", std::string("Foo"));
    original->push_back(std::move(foo));

    auto bar = std::make_unique<gtirb::Node>();
    bar->setLocalProperty("Name", std::string("Bar"));
    original->push_back(std::move(bar));

    EXPECT_EQ(size_t{2}, original->size());

    // Scope objects so they are destroyed
    {
        EXPECT_EQ(size_t{1}, original->getLocalPropertySize());
        EXPECT_EQ(std::string{"Value"},
                  boost::get<std::string>(original->getLocalProperty("Name")));

        // Serialize Out.
        std::ofstream ofs{tempPathString.c_str()};
        boost::archive::polymorphic_text_oarchive oa{ofs};
        EXPECT_TRUE(ofs.is_open());

        oa << original;

        EXPECT_NO_THROW(ofs.close());
        EXPECT_FALSE(ofs.is_open());
    }

    // Read it back in and re-test
    {
        auto serialized = std::make_shared<TypeParam>();

        // Serialize In.
        std::ifstream ifs{tempPathString.c_str()};
        boost::archive::polymorphic_text_iarchive ia{ifs};

        EXPECT_NO_THROW(ia >> serialized);

        EXPECT_NO_THROW(ifs.close());

        ASSERT_EQ(size_t{1}, serialized->getLocalPropertySize());
        EXPECT_EQ(std::string{"Value"},
                  boost::get<std::string>(serialized->getLocalProperty("Name")));
        EXPECT_EQ(original->getUUID(), serialized->getUUID());
        EXPECT_EQ(original->size(), serialized->size());
        EXPECT_EQ(sizeof(original), sizeof(serialized));
        EXPECT_EQ(sizeof(*original), sizeof(*serialized));
        EXPECT_EQ(typeid(*original), typeid(*serialized));

        ASSERT_EQ(size_t{2}, serialized->size());

        auto child0 = serialized->at(0);
        EXPECT_EQ(std::string{"Foo"}, boost::get<std::string>(child0->getLocalProperty("Name")));
        EXPECT_EQ(serialized.get(), child0->getNodeParent());

        auto child1 = serialized->at(1);
        EXPECT_EQ(std::string{"Bar"}, boost::get<std::string>(child1->getLocalProperty("Name")));
        EXPECT_EQ(serialized.get(), child1->getNodeParent());
    }
}

REGISTER_TYPED_TEST_CASE_P(TypedNodeTest,          //
                           ctor_0,                 //
                           clear,                  //
                           clearLocalProperties,   //
                           const_iterator,         //
                           iterator,               //
                           push_back,              //
                           removeLocalProperty,    //
                           setLocalProperties,     //
                           setLocalProperty,       //
                           setLocalPropertyReset,  //
                           size,                   //
                           uniqueUuids,            //
                           shared_from_this,       //
                           GetChildrenOfType,      //
                           serialize,              //
                           serializeBinary,        //
                           serializeFromSharedPtr, //
                           serializeFromSharedPtrWChildren);

INSTANTIATE_TYPED_TEST_CASE_P(Unit_Nodes,           // Instance name
                              TypedNodeTest,        // Test case name
                              TypeImplementations); // Type list