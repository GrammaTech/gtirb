#include <gtest/gtest.h>
#include <gtirb/Module.hpp>
#include <memory>

TEST(Unit_Module, ctor_0)
{
    EXPECT_NO_THROW(gtirb::Module());
}

TEST(Unit_Module, setBinaryPath)
{
    const std::string strPath("/home/gt/irb/foo");
    auto m = std::make_shared<gtirb::Module>();

    EXPECT_NO_THROW(m->setBinaryPath(strPath));

    auto path = m->getBinaryPath();
    EXPECT_EQ(boost::filesystem::path(strPath), path);
}

TEST(Unit_Module, getFileFormatDefault)
{
    auto m = std::make_shared<gtirb::Module>();
    EXPECT_EQ(gtirb::FileFormat::Undefined, m->getFileFormat());
}

TEST(Unit_Module, setFileFormat)
{
    auto m = std::make_shared<gtirb::Module>();

    EXPECT_NO_THROW(m->setFileFormat(gtirb::FileFormat::COFF));
    EXPECT_EQ(gtirb::FileFormat::COFF, m->getFileFormat());

    EXPECT_NO_THROW(m->setFileFormat(gtirb::FileFormat::MACHO));
    EXPECT_EQ(gtirb::FileFormat::MACHO, m->getFileFormat());

    EXPECT_NO_THROW(m->setFileFormat(gtirb::FileFormat::Undefined));
    EXPECT_EQ(gtirb::FileFormat::Undefined, m->getFileFormat());
}

TEST(Unit_Module, getRebaseDeltaDefault)
{
    auto m = std::make_shared<gtirb::Module>();
    EXPECT_EQ(int64_t{0}, m->getRebaseDelta());
}

TEST(Unit_Module, setRebaseDelta)
{
    auto m = std::make_shared<gtirb::Module>();

    EXPECT_NO_THROW(m->setRebaseDelta(1));
    EXPECT_EQ(int64_t{1}, m->getRebaseDelta());

    EXPECT_NO_THROW(m->setRebaseDelta(-1));
    EXPECT_EQ(int64_t{-1}, m->getRebaseDelta());

    EXPECT_NO_THROW(m->setRebaseDelta(std::numeric_limits<int64_t>::max()));
    EXPECT_EQ(std::numeric_limits<int64_t>::max(), m->getRebaseDelta());

    EXPECT_NO_THROW(m->setRebaseDelta(std::numeric_limits<int64_t>::min()));
    EXPECT_EQ(std::numeric_limits<int64_t>::min(), m->getRebaseDelta());

    EXPECT_NO_THROW(m->setRebaseDelta(std::numeric_limits<int64_t>::lowest()));
    EXPECT_EQ(std::numeric_limits<int64_t>::lowest(), m->getRebaseDelta());
}

TEST(Unit_Module, getEAMinMaxDefault)
{
    auto m = std::make_shared<gtirb::Module>();

    EXPECT_NO_THROW(m->getEAMinMax());
    EXPECT_EQ(gtirb::EA{}, m->getEAMinMax().first);
    EXPECT_EQ(gtirb::EA{}, m->getEAMinMax().second);
}

TEST(Unit_Module, setEAMinMax)
{
    auto m = std::make_shared<gtirb::Module>();
    
    gtirb::EA minimum{64};
    gtirb::EA maximum{1024};
    
    EXPECT_TRUE(m->setEAMinMax({minimum, maximum}));
    EXPECT_EQ(minimum, m->getEAMinMax().first);
    EXPECT_EQ(maximum, m->getEAMinMax().second);

    EXPECT_FALSE(m->setEAMinMax({maximum, minimum}));
    EXPECT_EQ(gtirb::EA{}, m->getEAMinMax().first);
    EXPECT_EQ(gtirb::EA{}, m->getEAMinMax().second);
}

TEST(Unit_Module, getPreferredEADefault)
{
    auto m = std::make_shared<gtirb::Module>();

    EXPECT_NO_THROW(m->getPreferredEA());
    EXPECT_EQ(gtirb::EA{}, m->getPreferredEA());
}

TEST(Unit_Module, setPreferredEA)
{
    auto m = std::make_shared<gtirb::Module>();
    const gtirb::EA preferred{64};

    EXPECT_NO_THROW(m->getPreferredEA());
    EXPECT_NO_THROW(m->setPreferredEA(preferred));

    EXPECT_EQ(preferred, m->getPreferredEA());
}