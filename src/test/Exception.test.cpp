#include <gtirb/Exception.hpp>
#include <gtest/gtest.h>

TEST(Unit_Exception, FileName)
{
	auto e = gtirb::Exception{};
	const int lineNumber{__LINE__};
	const std::string fileName{__FILE__};

	EXPECT_NO_THROW(e.setLocation(fileName, lineNumber));

	auto location = e.getLocation();
	EXPECT_EQ(fileName, location.first);
}

TEST(Unit_Exception, LineNumber)
{
	auto e = gtirb::Exception{};
	const int lineNumber{__LINE__};
	const std::string fileName{__FILE__};

	EXPECT_NO_THROW(e.setLocation(fileName, lineNumber));

	auto location = e.getLocation();
	EXPECT_EQ(lineNumber, location.second);
}
