#include <gtest/gtest.h>
#include <proto/Symbol.pb.h>
#include <gtirb/Data.hpp>
#include <gtirb/Instruction.hpp>
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

TEST(Unit_Symbol, setStorageKind)
{
    const gtirb::Symbol::StorageKind value{gtirb::Symbol::StorageKind::Static};

    auto node = std::make_unique<gtirb::Symbol>();
    EXPECT_NO_THROW(node->getStorageKind());
    EXPECT_EQ(gtirb::Symbol::StorageKind{}, node->getStorageKind());

    EXPECT_NO_THROW(node->setStorageKind(value));
    EXPECT_EQ(value, node->getStorageKind());
}

TEST(Unit_Symbol, setReferent)
{
    Symbol sym;
    Data data;
    Instruction inst;

    sym.setReferent(data);
    EXPECT_EQ(&*sym.getDataReferent(), &data);
    EXPECT_FALSE(sym.getCodeReferent());

    sym.setReferent(inst);
    EXPECT_EQ(&*sym.getCodeReferent(), &inst);
    // Setting code referent clears data referent
    EXPECT_FALSE(sym.getDataReferent());

    sym.setReferent(data);
    EXPECT_EQ(&*sym.getDataReferent(), &data);
    // Setting data referent clears code referent
    EXPECT_FALSE(sym.getCodeReferent());
}

TEST(Unit_Symbol, protobufRoundTrip)
{
    Symbol result;
    proto::Symbol message;
    UUID dataUUID;

    {
        Symbol original(EA(1), "test");
        original.setStorageKind(Symbol::StorageKind::Static);

        Data data;
        dataUUID = data.getUUID();
        original.setReferent(data);

        original.toProtobuf(&message);
    }

    result.fromProtobuf(message);

    EXPECT_EQ(result.getEA(), EA(1));
    EXPECT_EQ(result.getName(), "test");
    EXPECT_EQ(result.getStorageKind(), Symbol::StorageKind::Static);
    EXPECT_EQ(result.getDataReferent().getUUID(), dataUUID);
    EXPECT_EQ(result.getCodeReferent().getUUID(), UUID());
}
