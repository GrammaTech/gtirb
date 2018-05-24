#include <gtest/gtest.h>
#include <gtirb/Exception.hpp>
#include <gtirb/LogicError.hpp>
#include <gtirb/NodeError.hpp>
#include <gtirb/RuntimeError.hpp>
#include <memory>

using testing::Types;

typedef Types<gtirb::Exception, gtirb::LogicError, gtirb::NodeError, gtirb::RuntimeError>
    TypeImplementations;

// ----------------------------------------------------------------------------
// Typed test fixture.

template <class T>
class TypedExceptionTest : public testing::Test
{
protected:
    TypedExceptionTest() = default;
    virtual ~TypedExceptionTest() = default;
};

TYPED_TEST_CASE_P(TypedExceptionTest);

// ----------------------------------------------------------------------------
// Tests to run on all types.

TYPED_TEST_P(TypedExceptionTest, ctor_0)
{
    EXPECT_NO_THROW(TypeParam{});
}

TYPED_TEST_P(TypedExceptionTest, ctor_1)
{
    const std::string fileName{__FILE__};
    const int lineNumber{__LINE__};

    auto e = TypeParam("Test Exception.", fileName, lineNumber);

    auto location = e.getLocation();
    EXPECT_EQ(fileName, location.first);
    EXPECT_EQ(lineNumber, location.second);
}

TYPED_TEST_P(TypedExceptionTest, InheritanceStd)
{
    auto e = std::make_unique<TypeParam>();
    EXPECT_TRUE(e != nullptr);

    auto stdException = dynamic_cast<std::exception*>(e.get());
    EXPECT_TRUE(stdException != nullptr) << "T should inherit from std::exception.";
}

TYPED_TEST_P(TypedExceptionTest, InheritanceGtirb)
{
    auto e = std::make_unique<TypeParam>();
    EXPECT_TRUE(e != nullptr);

    auto stdException = dynamic_cast<gtirb::Exception*>(e.get());
    EXPECT_TRUE(stdException != nullptr) << "T should inherit from gtirb::Exception.";
}

TYPED_TEST_P(TypedExceptionTest, FileName)
{
    auto e = TypeParam{};

    const std::string fileName{__FILE__};
    const int lineNumber{__LINE__};

    EXPECT_NO_THROW(e.setLocation(fileName, lineNumber));

    auto location = e.getLocation();
    EXPECT_EQ(fileName, location.first);
}

TYPED_TEST_P(TypedExceptionTest, LineNumber)
{
    auto e = TypeParam{};

    const std::string fileName{__FILE__};
    const int lineNumber{__LINE__};

    EXPECT_NO_THROW(e.setLocation(fileName, lineNumber));

    auto location = e.getLocation();
    EXPECT_EQ(lineNumber, location.second);
}

TYPED_TEST_P(TypedExceptionTest, What)
{
    const auto e = TypeParam{"Test Exception.", __FILE__, __LINE__};
    const auto what = std::string{e.what()};
    EXPECT_FALSE(what.empty());
}

TYPED_TEST_P(TypedExceptionTest, Assignment)
{
    const std::string fileName{__FILE__};
    const int lineNumber{__LINE__};

    auto e = TypeParam("Test Exception.", fileName, lineNumber);

    auto location = e.getLocation();
    EXPECT_EQ(fileName, location.first);
    EXPECT_EQ(lineNumber, location.second);

    auto eCopy = e;

    location = eCopy.getLocation();
    EXPECT_EQ(fileName, location.first);
    EXPECT_EQ(lineNumber, location.second);
}

TYPED_TEST_P(TypedExceptionTest, CopyCtor)
{
    const std::string fileName{__FILE__};
    const int lineNumber{__LINE__};

    auto e = TypeParam("Test Exception.", fileName, lineNumber);

    auto location = e.getLocation();
    EXPECT_EQ(fileName, location.first);
    EXPECT_EQ(lineNumber, location.second);

    auto eCopy(e);

    location = eCopy.getLocation();
    EXPECT_EQ(fileName, location.first);
    EXPECT_EQ(lineNumber, location.second);
}

REGISTER_TYPED_TEST_CASE_P(TypedExceptionTest, ctor_0, ctor_1, InheritanceStd, InheritanceGtirb,
                           FileName, LineNumber, Assignment, What, CopyCtor);

INSTANTIATE_TYPED_TEST_CASE_P(Unit_Exceptions,      // Instance name
                              TypedExceptionTest,   // Test case name
                              TypeImplementations); // Type list
