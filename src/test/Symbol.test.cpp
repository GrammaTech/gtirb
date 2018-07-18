#include <gtest/gtest.h>
#include <proto/Symbol.pb.h>
#include <boost/archive/polymorphic_text_iarchive.hpp>
#include <boost/archive/polymorphic_text_oarchive.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Symbol.hpp>
#include <memory>
#include <sstream>

using namespace gtirb;

TEST(Unit_Symbol, ctor_0)
{
    EXPECT_NO_THROW(gtirb::Symbol());
}

TEST(Unit_Symbol, setName)
{
    const std::string value{"Foo"};

    auto node = std::make_unique<gtirb::Symbol>();
    EXPECT_NO_THROW(node->getName());
    EXPECT_TRUE(node->getName().empty());

    EXPECT_NO_THROW(node->setName(value));
    EXPECT_EQ(value, node->getName());
}

TEST(Unit_Symbol, setEA)
{
    const gtirb::EA value{22678};

    auto node = std::make_unique<gtirb::Symbol>();
    EXPECT_NO_THROW(node->getEA());
    EXPECT_EQ(gtirb::EA{}, node->getEA());

    EXPECT_NO_THROW(node->setEA(value));
    EXPECT_EQ(value, node->getEA());
}

TEST(Unit_Symbol, setType)
{
    const gtirb::Symbol::Type value{gtirb::Symbol::Type::DualCode};

    auto node = std::make_unique<gtirb::Symbol>();
    EXPECT_NO_THROW(node->getType());
    EXPECT_EQ(gtirb::Symbol::Type{}, node->getType());

    EXPECT_NO_THROW(node->setType(value));
    EXPECT_EQ(value, node->getType());
}

TEST(Unit_Symbol, setOffset)
{
    const int64_t value{22678};

    auto node = std::make_unique<gtirb::Symbol>();
    EXPECT_NO_THROW(node->getOffset());
    EXPECT_EQ(int64_t{0}, node->getOffset());

    EXPECT_NO_THROW(node->setOffset(value));
    EXPECT_EQ(value, node->getOffset());
}

TEST(Unit_Symbol, setElementSize)
{
    const int64_t value{22678};

    auto node = std::make_unique<gtirb::Symbol>();
    EXPECT_NO_THROW(node->getElementSize());
    EXPECT_EQ(int64_t{0}, node->getElementSize());

    EXPECT_NO_THROW(node->setElementSize(value));
    EXPECT_EQ(value, node->getElementSize());
}

TEST(Unit_Symbol, setBitSize)
{
    const int64_t value{22678};

    auto node = std::make_unique<gtirb::Symbol>();
    EXPECT_NO_THROW(node->getBitSize());
    EXPECT_EQ(int64_t{0}, node->getBitSize());

    EXPECT_NO_THROW(node->setBitSize(value));
    EXPECT_EQ(value, node->getBitSize());
}

TEST(Unit_Symbol, setDeclarationKind)
{
    const gtirb::Symbol::DeclarationKind value{gtirb::Symbol::DeclarationKind::IndirectFunc};

    auto node = std::make_unique<gtirb::Symbol>();
    EXPECT_NO_THROW(node->getDeclarationKind());
    EXPECT_EQ(gtirb::Symbol::DeclarationKind{}, node->getDeclarationKind());

    EXPECT_NO_THROW(node->setDeclarationKind(value));
    EXPECT_EQ(value, node->getDeclarationKind());
}

TEST(Unit_Symbol, setLinkType)
{
    const gtirb::Symbol::LinkType value{gtirb::Symbol::LinkType::ReadOnly};

    auto node = std::make_unique<gtirb::Symbol>();
    EXPECT_NO_THROW(node->getLinkType());
    EXPECT_EQ(gtirb::Symbol::LinkType{}, node->getLinkType());

    EXPECT_NO_THROW(node->setLinkType(value));
    EXPECT_EQ(value, node->getLinkType());
}

TEST(Unit_Symbol, setStorageKind)
{
    const gtirb::Symbol::StorageKind value{gtirb::Symbol::StorageKind::Static};

    auto node = std::make_unique<gtirb::Symbol>();
    EXPECT_NO_THROW(node->getStorageKind());
    EXPECT_EQ(gtirb::Symbol::StorageKind{}, node->getStorageKind());

    EXPECT_NO_THROW(node->setStorageKind(value));
    EXPECT_EQ(value, node->getStorageKind());
}

TEST(Unit_Symbol, setIsFormal)
{
    const bool value{true};

    auto node = std::make_unique<gtirb::Symbol>();
    EXPECT_NO_THROW(node->getIsFormal());
    EXPECT_EQ(false, node->getIsFormal());

    EXPECT_NO_THROW(node->setIsFormal(value));
    EXPECT_EQ(value, node->getIsFormal());
}

TEST(Unit_Symbol, setEnableForceName)
{
    const bool value{true};

    auto node = std::make_unique<gtirb::Symbol>();
    EXPECT_NO_THROW(node->getEnableForceName());
    EXPECT_EQ(false, node->getEnableForceName());

    EXPECT_NO_THROW(node->setEnableForceName(value));
    EXPECT_EQ(value, node->getEnableForceName());
}

TEST(Unit_Symbol, setEnableGapSize)
{
    const bool value{true};

    auto node = std::make_unique<gtirb::Symbol>();
    EXPECT_NO_THROW(node->getEnableGapSize());
    EXPECT_EQ(false, node->getEnableGapSize());

    EXPECT_NO_THROW(node->setEnableGapSize(value));
    EXPECT_EQ(value, node->getEnableGapSize());
}

TEST(Unit_Symbol, setIsNameOnly)
{
    const bool value{true};

    auto node = std::make_unique<gtirb::Symbol>();
    EXPECT_NO_THROW(node->getIsNameOnly());
    EXPECT_EQ(false, node->getIsNameOnly());

    EXPECT_NO_THROW(node->setIsNameOnly(value));
    EXPECT_EQ(value, node->getIsNameOnly());
}

TEST(Unit_Symbol, setIsGlobal)
{
    const bool value{true};

    auto node = std::make_unique<gtirb::Symbol>();
    EXPECT_NO_THROW(node->getIsGlobal());
    EXPECT_EQ(false, node->getIsGlobal());

    EXPECT_NO_THROW(node->setIsGlobal(value));
    EXPECT_EQ(value, node->getIsGlobal());
}

TEST(Unit_Symbol, protobufRoundTrip)
{
    Symbol original(EA(1), "test");
    original.setOffset(2);
    original.setElementSize(3);
    original.setBitSize(4);
    original.setType(Symbol::Type::Normal);
    original.setDeclarationKind(Symbol::DeclarationKind::Var);
    original.setLinkType(Symbol::LinkType::InitializedData);
    original.setStorageKind(Symbol::StorageKind::Static);
    original.setEnableForceName(true);
    original.setEnableGapSize(true);
    original.setIsFormal(true);
    original.setIsNameOnly(true);
    original.setIsGlobal(true);

    gtirb::Symbol result;
    proto::Symbol message;
    original.toProtobuf(&message);
    original.setUUID(); // Avoid UUID conflict
    result.fromProtobuf(message);

    EXPECT_EQ(result.getEA(), EA(1));
    EXPECT_EQ(result.getName(), "test");
    EXPECT_EQ(result.getOffset(), 2);
    EXPECT_EQ(result.getElementSize(), 3);
    EXPECT_EQ(result.getBitSize(), 4);
    EXPECT_EQ(result.getType(), Symbol::Type::Normal);
    EXPECT_EQ(result.getDeclarationKind(), Symbol::DeclarationKind::Var);
    EXPECT_EQ(result.getLinkType(), Symbol::LinkType::InitializedData);
    EXPECT_EQ(result.getStorageKind(), Symbol::StorageKind::Static);
    EXPECT_EQ(result.getEnableForceName(), true);
    EXPECT_EQ(result.getEnableGapSize(), true);
    EXPECT_EQ(result.getIsFormal(), true);
    EXPECT_EQ(result.getIsNameOnly(), true);
    EXPECT_EQ(result.getIsGlobal(), true);
}
