#include <gtest/gtest.h>
#include <gtirb/Exception.hpp>
#include <memory>

TEST(Unit_Exception, ctor_0)
{
    EXPECT_NO_THROW(gtirb::Exception{});
}

TEST(Unit_Exception, ctor_1)
{
    const std::string fileName{__FILE__};
    const int lineNumber{__LINE__};

    auto e = gtirb::Exception(fileName, lineNumber);

    auto location = e.getLocation();
    EXPECT_EQ(fileName, location.first);
    EXPECT_EQ(lineNumber, location.second);
}

TEST(Unit_Exception, inheritance)
{
    auto e = std::make_unique<gtirb::Exception>();
    EXPECT_TRUE(e != nullptr);

    auto stdException = dynamic_cast<std::exception*>(e.get());
    EXPECT_TRUE(stdException != nullptr) << "gtirb::Exception should inherit from std::exception.";
}

TEST(Unit_Exception, FileName)
{
    auto e = gtirb::Exception{};

    const std::string fileName{__FILE__};
    const int lineNumber{__LINE__};

    EXPECT_NO_THROW(e.setLocation(fileName, lineNumber));

    auto location = e.getLocation();
    EXPECT_EQ(fileName, location.first);
}

TEST(Unit_Exception, LineNumber)
{
    auto e = gtirb::Exception{};

    const std::string fileName{__FILE__};
    const int lineNumber{__LINE__};

    EXPECT_NO_THROW(e.setLocation(fileName, lineNumber));

    auto location = e.getLocation();
    EXPECT_EQ(lineNumber, location.second);
}

TEST(Unit_Exception, assignment)
{
    const std::string fileName{__FILE__};
    const int lineNumber{__LINE__};

    auto e = gtirb::Exception(fileName, lineNumber);

    auto location = e.getLocation();
    EXPECT_EQ(fileName, location.first);
    EXPECT_EQ(lineNumber, location.second);

    auto eCopy = e;

    location = eCopy.getLocation();
    EXPECT_EQ(fileName, location.first);
    EXPECT_EQ(lineNumber, location.second);
}

TEST(Unit_Exception, copyCtor)
{
    const std::string fileName{__FILE__};
    const int lineNumber{__LINE__};

    auto e = gtirb::Exception(fileName, lineNumber);

    auto location = e.getLocation();
    EXPECT_EQ(fileName, location.first);
    EXPECT_EQ(lineNumber, location.second);

    auto eCopy(e);

    location = eCopy.getLocation();
    EXPECT_EQ(fileName, location.first);
    EXPECT_EQ(lineNumber, location.second);
}

TEST(Unit_Exception, what)
{
    const auto e = gtirb::Exception{__FILE__, __LINE__};
    const auto what = std::string{e.what()};
    EXPECT_FALSE(what.empty());
}