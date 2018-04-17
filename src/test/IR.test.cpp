#include <gtest/gtest.h>
#include <gtirb/IR.hpp>
#include <memory>

TEST(Unit_IR, ctor_0)
{
    EXPECT_NO_THROW(gtirb::IR());
}
