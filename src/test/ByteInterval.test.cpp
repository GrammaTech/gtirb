//===- Block.test.cpp -------------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2020 GrammaTech, Inc.
//
//  This code is licensed under the MIT license. See the LICENSE file in the
//  project root for license terms.
//
//  This project is sponsored by the Office of Naval Research, One Liberty
//  Center, 875 N. Randolph Street, Arlington, VA 22203 under contract #
//  N68335-17-C-0700.  The content of the information does not necessarily
//  reflect the position or policy of the Government and no official
//  endorsement should be inferred.
//
//===----------------------------------------------------------------------===//
#include "SerializationTestHarness.hpp"
#include <gtirb/ByteInterval.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/proto/ByteInterval.pb.h>
#include <gtest/gtest.h>
#include <sstream>

using namespace gtirb;

static Context Ctx;

// Transforms a range of references into a range of pointers.
template <typename RangeTy> auto pointers(const RangeTy& Range) {
  auto GetPointer = [](auto& arg) { return &arg; };
  return boost::make_iterator_range(
      boost::make_transform_iterator(Range.begin(), GetPointer),
      boost::make_transform_iterator(Range.end(), GetPointer));
}

TEST(Unit_ByteInterval, noCopyMoveConstructors) {
  EXPECT_FALSE(std::is_copy_constructible_v<ByteInterval>);
  EXPECT_FALSE(std::is_move_constructible_v<ByteInterval>);
  EXPECT_FALSE(std::is_copy_assignable_v<ByteInterval>);
  EXPECT_FALSE(std::is_move_assignable_v<ByteInterval>);
}

TEST(Unit_ByteInterval, ctor) {
  EXPECT_NE(ByteInterval::Create(Ctx, Addr(1), 100), nullptr);
}

TEST(Unit_ByteInterval, gettersSetters) {
  auto* BI = ByteInterval::Create(Ctx, std::optional<Addr>(), 2);
  EXPECT_EQ(std::optional<Addr>(), BI->getAddress());
  EXPECT_EQ(2, BI->getSize());
  EXPECT_EQ(2, BI->getInitializedSize());
  EXPECT_EQ(BI->getSection(), nullptr);

  BI->setAddress(Addr(1));
  EXPECT_EQ(std::optional<Addr>(1), BI->getAddress());
  EXPECT_EQ(2, BI->getSize());
  EXPECT_EQ(2, BI->getInitializedSize());
  EXPECT_EQ(BI->getSection(), nullptr);

  BI->setSize(10);
  EXPECT_EQ(std::optional<Addr>(1), BI->getAddress());
  EXPECT_EQ(10, BI->getSize());
  EXPECT_EQ(2, BI->getInitializedSize());
  EXPECT_EQ(BI->getSection(), nullptr);

  BI->setInitializedSize(5);
  EXPECT_EQ(std::optional<Addr>(1), BI->getAddress());
  EXPECT_EQ(10, BI->getSize());
  EXPECT_EQ(5, BI->getInitializedSize());
  EXPECT_EQ(BI->getSection(), nullptr);

  BI->setSize(1);
  EXPECT_EQ(std::optional<Addr>(1), BI->getAddress());
  EXPECT_EQ(1, BI->getSize());
  EXPECT_EQ(1, BI->getInitializedSize());
  EXPECT_EQ(BI->getSection(), nullptr);

  BI->setInitializedSize(20);
  EXPECT_EQ(std::optional<Addr>(1), BI->getAddress());
  EXPECT_EQ(20, BI->getSize());
  EXPECT_EQ(20, BI->getInitializedSize());
  EXPECT_EQ(BI->getSection(), nullptr);
}

TEST(Unit_ByteInterval, protobufRoundTrip) {
  using STH = gtirb::SerializationTestHarness;
  // Test with fixed address.
  {
    std::stringstream ss;
    {
      Context InnerCtx;
      auto* Original = ByteInterval::Create(InnerCtx, Addr(1), 2);
      STH::save(*Original, ss);
    }
    auto* Result = STH::load<ByteInterval>(Ctx, ss);

    EXPECT_EQ(Result->getAddress(), std::optional<Addr>(1));
    EXPECT_EQ(Result->getSize(), 2);
    EXPECT_EQ(Result->getSection(), nullptr);
  }

  // Test without fixed address.
  {
    std::stringstream ss;
    {
      Context InnerCtx;
      auto* Original = ByteInterval::Create(InnerCtx, std::optional<Addr>(), 2);
      STH::save(*Original, ss);
    }
    auto* Result = STH::load<ByteInterval>(Ctx, ss);

    EXPECT_EQ(Result->getAddress(), std::optional<Addr>());
    EXPECT_EQ(Result->getSize(), 2);
    EXPECT_EQ(Result->getSection(), nullptr);
  }

  // Test with bytes.
  {
    std::stringstream ss;
    {
      Context InnerCtx;
      std::string Contents = "abcd";
      auto* Original = ByteInterval::Create(InnerCtx, std::optional<Addr>(),
                                            Contents.begin(), Contents.end());
      STH::save(*Original, ss);
    }
    auto* Result = STH::load<ByteInterval>(Ctx, ss);

    EXPECT_EQ(Result->getAddress(), std::optional<Addr>());
    EXPECT_EQ(Result->getSize(), 4);
    EXPECT_EQ(Result->getInitializedSize(), 4);
    EXPECT_EQ(Result->getSection(), nullptr);

    std::string ResultBytes;
    std::copy(Result->bytes_begin<char>(), Result->bytes_end<char>(),
              std::back_inserter(ResultBytes));
    ASSERT_EQ(ResultBytes, "abcd");
  }

  // Test truncating of unallocated bytes.
  {
    std::stringstream ss;
    {
      Context InnerCtx;
      std::string Contents = "abcd";
      auto* Original =
          ByteInterval::Create(InnerCtx, std::optional<Addr>(),
                               Contents.begin(), Contents.end(), 4, 2);
      STH::save(*Original, ss);
    }
    auto* Result = STH::load<ByteInterval>(Ctx, ss);

    EXPECT_EQ(Result->getAddress(), std::optional<Addr>());
    EXPECT_EQ(Result->getSize(), 4);
    EXPECT_EQ(Result->getInitializedSize(), 2);
    EXPECT_EQ(Result->getSection(), nullptr);

    std::string ResultBytes;
    std::copy(Result->bytes_begin<char>(), Result->bytes_end<char>(),
              std::back_inserter(ResultBytes));
    // Explode the string comparison, because the default one for std::string
    // does not like embedded NULs whatsoever.
    ASSERT_EQ(ResultBytes.size(), 4);
    ASSERT_EQ(ResultBytes[0], 'a');
    ASSERT_EQ(ResultBytes[1], 'b');
    ASSERT_EQ(ResultBytes[2], '\0');
    ASSERT_EQ(ResultBytes[3], '\0');
  }

  // Test with subobjects.
  {
    auto* Sym = Symbol::Create(Ctx, "test");

    std::stringstream ss;
    {
      Context InnerCtx;
      auto* Original = ByteInterval::Create(InnerCtx, Addr(0), 10);
      Original->addBlock<CodeBlock>(InnerCtx, 3, 1);
      Original->addBlock<CodeBlock>(InnerCtx, 6, 1);
      Original->addBlock<DataBlock>(InnerCtx, 6, 1);
      Original->addSymbolicExpression<SymAddrConst>(5, 8, Sym);
      STH::save(*Original, ss);
    }
    // Copy the stream so we can read from it a second time to do the
    // symbolic expressions.
    std::stringstream ss2;
    ss2 << ss.str();
    auto* Result = STH::load<ByteInterval>(Ctx, ss);

    EXPECT_EQ(std::distance(Result->blocks_begin(), Result->blocks_end()), 3);
    EXPECT_EQ(
        std::distance(Result->code_blocks_begin(), Result->code_blocks_end()),
        2);
    EXPECT_EQ(
        std::distance(Result->data_blocks_begin(), Result->data_blocks_end()),
        1);
    // Only after symbolicExpressionsFromProtobuf will sym exprs be populated.
    EXPECT_EQ(std::distance(Result->symbolic_expressions_begin(),
                            Result->symbolic_expressions_end()),
              0);

    constexpr auto getOffset = [](const Node& N) -> uint64_t {
      if (auto* B = dyn_cast_or_null<CodeBlock>(&N)) {
        return B->getOffset();
      }
      if (auto* D = dyn_cast_or_null<DataBlock>(&N)) {
        return D->getOffset();
      }
      assert(!"Found a non-block in a byte interval!");
      return 0;
    };

    EXPECT_EQ(getOffset(*Result->blocks_begin()), 3);
    EXPECT_EQ(getOffset(*std::next(Result->blocks_begin())), 6);
    EXPECT_EQ(getOffset(*std::next(std::next(Result->blocks_begin()))), 6);

    EXPECT_EQ(Result->code_blocks_begin()->getOffset(), 3);
    EXPECT_EQ(std::next(Result->code_blocks_begin())->getOffset(), 6);

    EXPECT_EQ(Result->data_blocks_begin()->getOffset(), 6);

    // Populate the sym exprs now.
    EXPECT_TRUE(STH::byteIntervalLoadSymbolicExpressions(Ctx, *Result, ss2));
    EXPECT_EQ(std::distance(Result->symbolic_expressions_begin(),
                            Result->symbolic_expressions_end()),
              1);

    EXPECT_EQ(Result->symbolic_expressions_begin()->getByteInterval(), Result);
    EXPECT_EQ(Result->symbolic_expressions_begin()->getOffset(), 5);
    EXPECT_TRUE(std::holds_alternative<SymAddrConst>(
        Result->symbolic_expressions_begin()->getSymbolicExpression()));
    EXPECT_EQ(std::get<SymAddrConst>(
                  Result->symbolic_expressions_begin()->getSymbolicExpression())
                  .Offset,
              8);
    EXPECT_EQ(std::get<SymAddrConst>(
                  Result->symbolic_expressions_begin()->getSymbolicExpression())
                  .Sym,
              Sym);
  }
}

TEST(Unit_ByteInterval, byteVector) {
  std::string Contents = "hello, world!";

  // Test all allocated bytes.
  {
    auto* BI = ByteInterval::Create(Ctx, std::optional<Addr>(),
                                    Contents.begin(), Contents.end());
    EXPECT_EQ(BI->getSize(), Contents.size());

    auto OriginalIt = Contents.begin();
    auto NewIt = BI->bytes_begin<char>();
    auto OriginalEnd = Contents.end();
    auto NewEnd = BI->bytes_end<char>();

    while (OriginalIt != OriginalEnd && NewIt != NewEnd) {
      EXPECT_EQ(*OriginalIt, *NewIt);
      ++OriginalIt;
      ++NewIt;
    }
    EXPECT_EQ(OriginalIt, OriginalEnd);
    EXPECT_EQ(NewIt, NewEnd);
  }

  // Test some unallocated bytes.
  {
    auto* BI = ByteInterval::Create(Ctx, std::optional<Addr>(),
                                    Contents.begin(), Contents.end(), 100);
    EXPECT_EQ(BI->getSize(), 100);

    auto OriginalIt = Contents.begin();
    auto NewIt = BI->bytes_begin<char>();
    auto OriginalEnd = Contents.end();
    auto NewEnd = BI->bytes_end<char>();

    while (OriginalIt != OriginalEnd && NewIt != NewEnd) {
      EXPECT_EQ(*OriginalIt, *NewIt);
      ++OriginalIt;
      ++NewIt;
    }
    EXPECT_EQ(OriginalIt, OriginalEnd);
    EXPECT_NE(NewIt, NewEnd);

    while (NewIt != NewEnd) {
      EXPECT_EQ(*NewIt, '\0');
      ++NewIt;
    }
    EXPECT_EQ(std::distance(BI->bytes_begin<char>(), NewIt), 100);
  }
}

template <typename T> static T str2(const char* S) {
  // TODO: is an endian conversion needed here?
  return *(const T*)S;
}

TEST(Unit_ByteInterval, byteVectorInts) {
  std::string Contents = "hello, world!!??";
  auto* BI = ByteInterval::Create(Ctx, std::optional<Addr>(), Contents.begin(),
                                  Contents.end());
  EXPECT_EQ(Contents.size(), 16);

  // 16 bits
  {
    std::vector<uint16_t> CompareTo = {
        str2<uint16_t>("he"), str2<uint16_t>("ll"), str2<uint16_t>("o,"),
        str2<uint16_t>(" w"), str2<uint16_t>("or"), str2<uint16_t>("ld"),
        str2<uint16_t>("!!"), str2<uint16_t>("??")};

    auto OriginalIt = CompareTo.begin();
    auto NewIt = BI->bytes_begin<uint16_t>();
    auto OriginalEnd = CompareTo.end();
    auto NewEnd = BI->bytes_end<uint16_t>();

    EXPECT_EQ(std::distance(NewIt, NewEnd), 8);
    while (OriginalIt != OriginalEnd && NewIt != NewEnd) {
      EXPECT_EQ(*OriginalIt, *NewIt);
      ++OriginalIt;
      ++NewIt;
    }
    EXPECT_EQ(OriginalIt, OriginalEnd);
    EXPECT_EQ(NewIt, NewEnd);
  }

  // 32 bits
  {
    std::vector<uint32_t> CompareTo = {
        str2<uint32_t>("hell"), str2<uint32_t>("o, w"), str2<uint32_t>("orld"),
        str2<uint32_t>("!!??")};

    auto OriginalIt = CompareTo.begin();
    auto NewIt = BI->bytes_begin<uint32_t>();
    auto OriginalEnd = CompareTo.end();
    auto NewEnd = BI->bytes_end<uint32_t>();

    EXPECT_EQ(std::distance(NewIt, NewEnd), 4);
    while (OriginalIt != OriginalEnd && NewIt != NewEnd) {
      EXPECT_EQ(*OriginalIt, *NewIt);
      ++OriginalIt;
      ++NewIt;
    }
    EXPECT_EQ(OriginalIt, OriginalEnd);
    EXPECT_EQ(NewIt, NewEnd);
  }

  // 64 bits
  {
    std::vector<uint64_t> CompareTo = {str2<uint64_t>("hello, w"),
                                       str2<uint64_t>("orld!!??")};

    auto OriginalIt = CompareTo.begin();
    auto NewIt = BI->bytes_begin<uint64_t>();
    auto OriginalEnd = CompareTo.end();
    auto NewEnd = BI->bytes_end<uint64_t>();

    EXPECT_EQ(std::distance(NewIt, NewEnd), 2);
    while (OriginalIt != OriginalEnd && NewIt != NewEnd) {
      EXPECT_EQ(*OriginalIt, *NewIt);
      ++OriginalIt;
      ++NewIt;
    }
    EXPECT_EQ(OriginalIt, OriginalEnd);
    EXPECT_EQ(NewIt, NewEnd);
  }
}

TEST(Unit_ByteInterval, byteVectorEndian) {
  std::string Contents = "hello, world!!??";
  auto* BI = ByteInterval::Create(Ctx, std::optional<Addr>(), Contents.begin(),
                                  Contents.end());
  EXPECT_EQ(Contents.size(), 16);

  // Test using little endian.
  {
    std::vector<uint16_t> CompareTo = {
        str2<uint16_t>("he"), str2<uint16_t>("ll"), str2<uint16_t>("o,"),
        str2<uint16_t>(" w"), str2<uint16_t>("or"), str2<uint16_t>("ld"),
        str2<uint16_t>("!!"), str2<uint16_t>("??")};

    auto OriginalIt = CompareTo.begin();
    auto NewIt = BI->bytes_begin<uint16_t>(boost::endian::order::little);
    auto OriginalEnd = CompareTo.end();
    auto NewEnd = BI->bytes_end<uint16_t>(boost::endian::order::little);

    EXPECT_EQ(std::distance(NewIt, NewEnd), 8);
    while (OriginalIt != OriginalEnd && NewIt != NewEnd) {
      EXPECT_EQ(*OriginalIt, *NewIt);
      ++OriginalIt;
      ++NewIt;
    }
    EXPECT_EQ(OriginalIt, OriginalEnd);
    EXPECT_EQ(NewIt, NewEnd);
  }

  // Test using big endian.
  {
    std::vector<uint16_t> CompareTo = {
        str2<uint16_t>("eh"), str2<uint16_t>("ll"), str2<uint16_t>(",o"),
        str2<uint16_t>("w "), str2<uint16_t>("ro"), str2<uint16_t>("dl"),
        str2<uint16_t>("!!"), str2<uint16_t>("??")};

    auto OriginalIt = CompareTo.begin();
    auto NewIt = BI->bytes_begin<uint16_t>(boost::endian::order::big);
    auto OriginalEnd = CompareTo.end();
    auto NewEnd = BI->bytes_end<uint16_t>(boost::endian::order::big);

    EXPECT_EQ(std::distance(NewIt, NewEnd), 8);
    while (OriginalIt != OriginalEnd && NewIt != NewEnd) {
      EXPECT_EQ(*OriginalIt, *NewIt);
      ++OriginalIt;
      ++NewIt;
    }
    EXPECT_EQ(OriginalIt, OriginalEnd);
    EXPECT_EQ(NewIt, NewEnd);
  }
}

TEST(Unit_ByteInterval, byteVectorInsert) {
  std::string Contents = "0123456789";
  auto* BI = ByteInterval::Create(Ctx, std::optional<Addr>(), Contents.begin(),
                                  Contents.end());
  {
    std::string toInsert = "abcd";
    BI->insertBytes<char>(BI->bytes_begin<char>(), toInsert.begin(),
                          toInsert.end());
    std::string Result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(Result));
    ASSERT_EQ(Result, "abcd0123456789");
  }

  {
    std::string toInsert = "efg";
    BI->insertBytes<char>(BI->bytes_begin<char>() + 6, toInsert.begin(),
                          toInsert.end());
    std::string Result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(Result));
    ASSERT_EQ(Result, "abcd01efg23456789");
  }

  {
    std::string toInsert = "hi";
    BI->insertBytes<char>(BI->bytes_end<char>(), toInsert.begin(),
                          toInsert.end());
    std::string Result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(Result));
    ASSERT_EQ(Result, "abcd01efg23456789hi");
  }

  // Test with larger, non-char values.
  {
    std::vector<uint32_t> toInsert = {str2<uint32_t>("(hel"),
                                      str2<uint32_t>("lo.)")};
    BI->insertBytes<uint32_t>(BI->bytes_begin<uint32_t>() + 1, toInsert.begin(),
                              toInsert.end());
    std::string Result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(Result));
    ASSERT_EQ(Result, "abcd(hello.)01efg23456789hi");
  }
}

TEST(Unit_ByteInterval, byteVectorErase) {
  std::string Contents = "0123456789";
  auto* BI = ByteInterval::Create(Ctx, std::optional<Addr>(), Contents.begin(),
                                  Contents.end());

  {
    BI->eraseBytes<char>(BI->bytes_begin<char>(), BI->bytes_begin<char>() + 2);
    std::string Result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(Result));
    ASSERT_EQ(Result, "23456789");
  }

  {
    BI->eraseBytes<char>(BI->bytes_begin<char>() + 4,
                         BI->bytes_begin<char>() + 5);
    std::string Result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(Result));
    ASSERT_EQ(Result, "2345789");
  }

  {
    BI->eraseBytes<char>(BI->bytes_begin<char>() + 6, BI->bytes_end<char>());
    std::string Result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(Result));
    ASSERT_EQ(Result, "234578");
  }

  // Test larger value types.
  {
    BI->eraseBytes<uint16_t>(BI->bytes_begin<uint16_t>() + 1,
                             BI->bytes_begin<uint16_t>() + 2);
    std::string Result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(Result));
    ASSERT_EQ(Result, "2378");
  }

  // Test clearing the bytes.
  {
    BI->eraseBytes<uint32_t>(BI->bytes_begin<uint32_t>(),
                             BI->bytes_end<uint32_t>());
    std::string Result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(Result));
    ASSERT_EQ(Result, "");
  }
}

TEST(Unit_ByteInterval, removeBlock) {
  auto* BI = ByteInterval::Create(Ctx, Addr(0), 10);

  auto* B1 = BI->addBlock<CodeBlock>(Ctx, 0, 1);
  auto* B2 = BI->addBlock<DataBlock>(Ctx, 1, 1);
  auto* B3 = BI->addBlock<CodeBlock>(Ctx, 2, 1);
  auto* B4 = BI->addBlock<DataBlock>(Ctx, 3, 1);

  {
    auto Begin = BI->code_blocks_begin(), End = BI->code_blocks_end();
    ASSERT_EQ(std::distance(Begin, End), 2);
    ASSERT_NE(std::find_if(Begin, End, [&](const auto& N) { return B1 == &N; }),
              End);
    ASSERT_NE(std::find_if(Begin, End, [&](const auto& N) { return B3 == &N; }),
              End);
  }

  {
    auto Begin = BI->data_blocks_begin(), End = BI->data_blocks_end();
    ASSERT_EQ(std::distance(Begin, End), 2);
    ASSERT_NE(std::find_if(Begin, End, [&](const auto& N) { return B2 == &N; }),
              End);
    ASSERT_NE(std::find_if(Begin, End, [&](const auto& N) { return B4 == &N; }),
              End);
  }

  BI->removeBlock(B1);

  {
    auto Begin = BI->code_blocks_begin(), End = BI->code_blocks_end();
    ASSERT_EQ(std::distance(Begin, End), 1);
    ASSERT_NE(std::find_if(Begin, End, [&](const auto& N) { return B3 == &N; }),
              End);
  }

  {
    auto Begin = BI->data_blocks_begin(), End = BI->data_blocks_end();
    ASSERT_EQ(std::distance(Begin, End), 2);
    ASSERT_NE(std::find_if(Begin, End, [&](const auto& N) { return B2 == &N; }),
              End);
    ASSERT_NE(std::find_if(Begin, End, [&](const auto& N) { return B4 == &N; }),
              End);
  }

  BI->removeBlock(B2);

  {
    auto Begin = BI->code_blocks_begin(), End = BI->code_blocks_end();
    ASSERT_EQ(std::distance(Begin, End), 1);
    ASSERT_NE(std::find_if(Begin, End, [&](const auto& N) { return B3 == &N; }),
              End);
  }

  {
    auto Begin = BI->data_blocks_begin(), End = BI->data_blocks_end();
    ASSERT_EQ(std::distance(Begin, End), 1);
    ASSERT_NE(std::find_if(Begin, End, [&](const auto& N) { return B4 == &N; }),
              End);
  }
}

TEST(Unit_ByteInterval, addSymbolicExpression) {
  auto* BI = ByteInterval::Create(Ctx, std::optional<Addr>(), 10);
  auto* S = Symbol::Create(Ctx, "test");

  {
    auto& SE = BI->addSymbolicExpression(0, SymAddrConst{1, S});
    ASSERT_TRUE(std::holds_alternative<SymAddrConst>(SE));
    ASSERT_EQ(std::get<SymAddrConst>(SE).Offset, 1);
    ASSERT_EQ(std::get<SymAddrConst>(SE).Sym, S);
  }

  {
    auto& SE = BI->addSymbolicExpression(1, SymStackConst{2, S});
    ASSERT_TRUE(std::holds_alternative<SymStackConst>(SE));
    ASSERT_EQ(std::get<SymStackConst>(SE).Offset, 2);
    ASSERT_EQ(std::get<SymStackConst>(SE).Sym, S);
  }

  {
    auto& SE = BI->addSymbolicExpression(2, SymAddrAddr{3, 4, S, S});
    ASSERT_TRUE(std::holds_alternative<SymAddrAddr>(SE));
    ASSERT_EQ(std::get<SymAddrAddr>(SE).Scale, 3);
    ASSERT_EQ(std::get<SymAddrAddr>(SE).Offset, 4);
    ASSERT_EQ(std::get<SymAddrAddr>(SE).Sym1, S);
    ASSERT_EQ(std::get<SymAddrAddr>(SE).Sym2, S);
  }

  {
    auto& SE = BI->addSymbolicExpression(3, SymAddrConst{4, S});
    ASSERT_TRUE(std::holds_alternative<SymAddrConst>(SE));
    ASSERT_EQ(std::get<SymAddrConst>(SE).Offset, 4);
    ASSERT_EQ(std::get<SymAddrConst>(SE).Sym, S);
  }

  {
    auto& SE = BI->addSymbolicExpression(4, SymStackConst{5, S});
    ASSERT_TRUE(std::holds_alternative<SymStackConst>(SE));
    ASSERT_EQ(std::get<SymStackConst>(SE).Offset, 5);
    ASSERT_EQ(std::get<SymStackConst>(SE).Sym, S);
  }

  {
    auto& SE = BI->addSymbolicExpression(5, SymAddrAddr{6, 7, S, S});
    ASSERT_TRUE(std::holds_alternative<SymAddrAddr>(SE));
    ASSERT_EQ(std::get<SymAddrAddr>(SE).Scale, 6);
    ASSERT_EQ(std::get<SymAddrAddr>(SE).Offset, 7);
    ASSERT_EQ(std::get<SymAddrAddr>(SE).Sym1, S);
    ASSERT_EQ(std::get<SymAddrAddr>(SE).Sym2, S);
  }

  {
    auto Range = BI->symbolic_expressions();
    ASSERT_EQ(std::distance(Range.begin(), Range.end()), 6);
    for (uint64_t i = 0; i < 6; ++i)
      EXPECT_EQ(std::next(Range.begin(), i)->getOffset(), i);
  }
}

TEST(Unit_ByteInterval, updateSymbolicExpression) {
  auto* BI = ByteInterval::Create(Ctx, std::optional<Addr>(), 10);
  auto* S = Symbol::Create(Ctx, "test");
  BI->addSymbolicExpression(0, SymAddrConst{1, S});

  auto& SE = BI->addSymbolicExpression(0, SymAddrConst{2, S});
  ASSERT_TRUE(std::holds_alternative<SymAddrConst>(SE));
  ASSERT_EQ(std::get<SymAddrConst>(SE).Offset, 2);
}

TEST(Unit_ByteInterval, findSymbolicExpressionsAtOffset) {
  auto* BI = ByteInterval::Create(Ctx, 10);
  auto* S1 = Symbol::Create(Ctx, "A");
  auto* S2 = Symbol::Create(Ctx, "B");
  BI->addSymbolicExpression(2, SymAddrConst{0, S1});
  BI->addSymbolicExpression(4, SymStackConst{5, S2});

  auto Range = BI->findSymbolicExpressionsAtOffset(2);
  ASSERT_EQ(std::distance(Range.begin(), Range.end()), 1);

  auto FoundSymbolicExpressionElement = Range.front();
  const SymbolicExpression* SE =
      &FoundSymbolicExpressionElement.getSymbolicExpression();
  ASSERT_TRUE(std::holds_alternative<SymAddrConst>(*SE));
  EXPECT_EQ(std::get<SymAddrConst>(*SE).Offset, 0);
  EXPECT_EQ(std::get<SymAddrConst>(*SE).Sym, S1);

  Range = BI->findSymbolicExpressionsAtOffset(4);
  ASSERT_EQ(std::distance(Range.begin(), Range.end()), 1);

  FoundSymbolicExpressionElement = Range.front();
  SE = &FoundSymbolicExpressionElement.getSymbolicExpression();
  ASSERT_TRUE(std::holds_alternative<SymStackConst>(*SE));
  EXPECT_EQ(std::get<SymStackConst>(*SE).Offset, 5);
  EXPECT_EQ(std::get<SymStackConst>(*SE).Sym, S2);

  Range = BI->findSymbolicExpressionsAtOffset(6);
  ASSERT_TRUE(Range.empty());

  Range = BI->findSymbolicExpressionsAtOffset(0, 10);
  ASSERT_EQ(std::distance(Range.begin(), Range.end()), 2);

  FoundSymbolicExpressionElement = Range.front();
  SE = &FoundSymbolicExpressionElement.getSymbolicExpression();
  ASSERT_TRUE(std::holds_alternative<SymAddrConst>(*SE));
  EXPECT_EQ(std::get<SymAddrConst>(*SE).Sym, S1);

  FoundSymbolicExpressionElement = Range.back();
  SE = &FoundSymbolicExpressionElement.getSymbolicExpression();
  ASSERT_TRUE(std::holds_alternative<SymStackConst>(*SE));
  EXPECT_EQ(std::get<SymStackConst>(*SE).Sym, S2);
}

TEST(Unit_ByteInterval, removeSymbolicExpression) {
  auto* BI = ByteInterval::Create(Ctx, 10);
  auto* S = Symbol::Create(Ctx, "test");
  BI->addSymbolicExpression(5, SymAddrConst{0, S});

  ASSERT_FALSE(BI->symbolic_expressions().empty());

  BI->removeSymbolicExpression(5);

  EXPECT_TRUE(BI->symbolic_expressions().empty());
}

TEST(Unit_ByteInterval, findBlocksOn) {
  auto* BI = ByteInterval::Create(Ctx, 10);
  auto* B1 = BI->addBlock<CodeBlock>(Ctx, 0, 2);
  auto* B2 = BI->addBlock<CodeBlock>(Ctx, 5, 3);
  const ByteInterval* CBI = BI;

  // Querying an out-of-bounds offset produces an empty range.

  auto BlockOffsetRange = BI->findBlocksOnOffset(10);
  EXPECT_TRUE(BlockOffsetRange.empty());

  auto ConstBlockOffsetRange = CBI->findBlocksOnOffset(10);
  EXPECT_TRUE(ConstBlockOffsetRange.empty());

  // Querying an in-bounds offset returns the appropriate block.

  BlockOffsetRange = BI->findBlocksOnOffset(6);
  ASSERT_EQ(std::distance(BlockOffsetRange.begin(), BlockOffsetRange.end()), 1);
  EXPECT_EQ(&*BlockOffsetRange.begin(), B2);

  ConstBlockOffsetRange = CBI->findBlocksOnOffset(6);
  ASSERT_EQ(
      std::distance(ConstBlockOffsetRange.begin(), ConstBlockOffsetRange.end()),
      1);
  EXPECT_EQ(&*ConstBlockOffsetRange.begin(), B2);

  // Address queries produce empty ranges if the ByteInterval has no address.

  auto BlockAddrRange = BI->findBlocksOn(Addr(6));
  EXPECT_TRUE(BlockAddrRange.empty());

  auto ConstBlockAddrRange = CBI->findBlocksOn(Addr(6));
  EXPECT_TRUE(ConstBlockAddrRange.empty());

  // Once the ByteInterval has an address, address queries are non-empty.

  BI->setAddress(Addr(0));
  BlockAddrRange = BI->findBlocksOn(Addr(6));
  ASSERT_EQ(std::distance(BlockAddrRange.begin(), BlockAddrRange.end()), 1);
  EXPECT_EQ(&*BlockAddrRange.begin(), B2);

  ConstBlockAddrRange = CBI->findBlocksOn(Addr(6));
  ASSERT_EQ(
      std::distance(ConstBlockAddrRange.begin(), ConstBlockAddrRange.end()), 1);
  EXPECT_EQ(&*ConstBlockAddrRange.begin(), B2);

  // If the address changes, the returned blocks may change.

  BI->setAddress(Addr(5));
  BlockAddrRange = BI->findBlocksOn(Addr(6));
  ASSERT_EQ(std::distance(BlockAddrRange.begin(), BlockAddrRange.end()), 1);
  EXPECT_EQ(&*BlockAddrRange.begin(), B1);

  ConstBlockAddrRange = CBI->findBlocksOn(Addr(6));
  ASSERT_EQ(
      std::distance(ConstBlockAddrRange.begin(), ConstBlockAddrRange.end()), 1);
  EXPECT_EQ(&*ConstBlockAddrRange.begin(), B1);

  // Querying offsets is not affected by address changes.

  BlockOffsetRange = BI->findBlocksOnOffset(6);
  ASSERT_EQ(std::distance(BlockOffsetRange.begin(), BlockOffsetRange.end()), 1);
  EXPECT_EQ(&*BlockOffsetRange.begin(), B2);

  ConstBlockOffsetRange = CBI->findBlocksOnOffset(6);
  ASSERT_EQ(
      std::distance(ConstBlockOffsetRange.begin(), ConstBlockOffsetRange.end()),
      1);
  EXPECT_EQ(&*ConstBlockOffsetRange.begin(), B2);

  // If the block size changes, queries will be affected.

  B1->setSize(10);

  BlockOffsetRange = BI->findBlocksOnOffset(6);
  ASSERT_EQ(std::distance(BlockOffsetRange.begin(), BlockOffsetRange.end()), 2);
  EXPECT_EQ(&*std::next(BlockOffsetRange.begin(), 0), B1);
  EXPECT_EQ(&*std::next(BlockOffsetRange.begin(), 1), B2);

  ConstBlockOffsetRange = CBI->findBlocksOnOffset(6);
  ASSERT_EQ(
      std::distance(ConstBlockOffsetRange.begin(), ConstBlockOffsetRange.end()),
      2);
  EXPECT_EQ(&*std::next(ConstBlockOffsetRange.begin(), 0), B1);
  EXPECT_EQ(&*std::next(ConstBlockOffsetRange.begin(), 1), B2);
}

TEST(Unit_ByteInterval, findCodeBlocksOn) {
  auto* BI = ByteInterval::Create(Ctx, 10);
  auto* B1 = BI->addBlock<CodeBlock>(Ctx, 0, 2);
  auto* B2 = BI->addBlock<CodeBlock>(Ctx, 5, 3);
  const ByteInterval* CBI = BI;

  // Querying an out-of-bounds offset produces an empty range.

  auto BlockOffsetRange = BI->findCodeBlocksOnOffset(10);
  EXPECT_TRUE(BlockOffsetRange.empty());

  auto ConstBlockOffsetRange = CBI->findCodeBlocksOnOffset(10);
  EXPECT_TRUE(ConstBlockOffsetRange.empty());

  // Querying an in-bounds offset returns the appropriate block.

  BlockOffsetRange = BI->findCodeBlocksOnOffset(6);
  ASSERT_EQ(std::distance(BlockOffsetRange.begin(), BlockOffsetRange.end()), 1);
  EXPECT_EQ(&*BlockOffsetRange.begin(), B2);

  ConstBlockOffsetRange = CBI->findCodeBlocksOnOffset(6);
  ASSERT_EQ(
      std::distance(ConstBlockOffsetRange.begin(), ConstBlockOffsetRange.end()),
      1);
  EXPECT_EQ(&*ConstBlockOffsetRange.begin(), B2);

  // Address queries produce empty ranges if the ByteInterval has no address.

  auto BlockAddrRange = BI->findCodeBlocksOn(Addr(6));
  EXPECT_TRUE(BlockAddrRange.empty());

  auto ConstBlockAddrRange = CBI->findCodeBlocksOn(Addr(6));
  EXPECT_TRUE(ConstBlockAddrRange.empty());

  // Once the ByteInterval has an address, address queries are non-empty.

  BI->setAddress(Addr(0));
  BlockAddrRange = BI->findCodeBlocksOn(Addr(6));
  ASSERT_EQ(std::distance(BlockAddrRange.begin(), BlockAddrRange.end()), 1);
  EXPECT_EQ(&*BlockAddrRange.begin(), B2);

  ConstBlockAddrRange = CBI->findCodeBlocksOn(Addr(6));
  ASSERT_EQ(
      std::distance(ConstBlockAddrRange.begin(), ConstBlockAddrRange.end()), 1);
  EXPECT_EQ(&*ConstBlockAddrRange.begin(), B2);

  // If the address changes, the returned blocks may change.

  BI->setAddress(Addr(5));
  BlockAddrRange = BI->findCodeBlocksOn(Addr(6));
  ASSERT_EQ(std::distance(BlockAddrRange.begin(), BlockAddrRange.end()), 1);
  EXPECT_EQ(&*BlockAddrRange.begin(), B1);

  ConstBlockAddrRange = CBI->findCodeBlocksOn(Addr(6));
  ASSERT_EQ(
      std::distance(ConstBlockAddrRange.begin(), ConstBlockAddrRange.end()), 1);
  EXPECT_EQ(&*ConstBlockAddrRange.begin(), B1);

  // Querying offsets is not affected by address changes.

  BlockOffsetRange = BI->findCodeBlocksOnOffset(6);
  ASSERT_EQ(std::distance(BlockOffsetRange.begin(), BlockOffsetRange.end()), 1);
  EXPECT_EQ(&*BlockOffsetRange.begin(), B2);

  ConstBlockOffsetRange = CBI->findCodeBlocksOnOffset(6);
  ASSERT_EQ(
      std::distance(ConstBlockOffsetRange.begin(), ConstBlockOffsetRange.end()),
      1);
  EXPECT_EQ(&*ConstBlockOffsetRange.begin(), B2);

  // If the block size changes, queries will be affected.

  B1->setSize(10);

  BlockOffsetRange = BI->findCodeBlocksOnOffset(6);
  ASSERT_EQ(std::distance(BlockOffsetRange.begin(), BlockOffsetRange.end()), 2);
  EXPECT_EQ(&*std::next(BlockOffsetRange.begin(), 0), B1);
  EXPECT_EQ(&*std::next(BlockOffsetRange.begin(), 1), B2);

  ConstBlockOffsetRange = CBI->findCodeBlocksOnOffset(6);
  ASSERT_EQ(
      std::distance(ConstBlockOffsetRange.begin(), ConstBlockOffsetRange.end()),
      2);
  EXPECT_EQ(&*std::next(ConstBlockOffsetRange.begin(), 0), B1);
  EXPECT_EQ(&*std::next(ConstBlockOffsetRange.begin(), 1), B2);
}

TEST(Unit_ByteInterval, findDataBlocksOn) {
  auto* BI = ByteInterval::Create(Ctx, 10);
  auto* B1 = BI->addBlock<DataBlock>(Ctx, 0, 2);
  auto* B2 = BI->addBlock<DataBlock>(Ctx, 5, 3);
  const ByteInterval* CBI = BI;

  // Querying an out-of-bounds offset produces an empty range.

  auto BlockOffsetRange = BI->findDataBlocksOnOffset(10);
  EXPECT_TRUE(BlockOffsetRange.empty());

  auto ConstBlockOffsetRange = CBI->findDataBlocksOnOffset(10);
  EXPECT_TRUE(ConstBlockOffsetRange.empty());

  // Querying an in-bounds offset returns the appropriate block.

  BlockOffsetRange = BI->findDataBlocksOnOffset(6);
  ASSERT_EQ(std::distance(BlockOffsetRange.begin(), BlockOffsetRange.end()), 1);
  EXPECT_EQ(&*BlockOffsetRange.begin(), B2);

  ConstBlockOffsetRange = CBI->findDataBlocksOnOffset(6);
  ASSERT_EQ(
      std::distance(ConstBlockOffsetRange.begin(), ConstBlockOffsetRange.end()),
      1);
  EXPECT_EQ(&*ConstBlockOffsetRange.begin(), B2);

  // Address queries produce empty ranges if the ByteInterval has no address.

  auto BlockAddrRange = BI->findDataBlocksOn(Addr(6));
  EXPECT_TRUE(BlockAddrRange.empty());

  auto ConstBlockAddrRange = CBI->findDataBlocksOn(Addr(6));
  EXPECT_TRUE(ConstBlockAddrRange.empty());

  // Once the ByteInterval has an address, address queries are non-empty.

  BI->setAddress(Addr(0));
  BlockAddrRange = BI->findDataBlocksOn(Addr(6));
  ASSERT_EQ(std::distance(BlockAddrRange.begin(), BlockAddrRange.end()), 1);
  EXPECT_EQ(&*BlockAddrRange.begin(), B2);

  ConstBlockAddrRange = CBI->findDataBlocksOn(Addr(6));
  ASSERT_EQ(
      std::distance(ConstBlockAddrRange.begin(), ConstBlockAddrRange.end()), 1);
  EXPECT_EQ(&*ConstBlockAddrRange.begin(), B2);

  // If the address changes, the returned blocks may change.

  BI->setAddress(Addr(5));
  BlockAddrRange = BI->findDataBlocksOn(Addr(6));
  ASSERT_EQ(std::distance(BlockAddrRange.begin(), BlockAddrRange.end()), 1);
  EXPECT_EQ(&*BlockAddrRange.begin(), B1);

  ConstBlockAddrRange = CBI->findDataBlocksOn(Addr(6));
  ASSERT_EQ(
      std::distance(ConstBlockAddrRange.begin(), ConstBlockAddrRange.end()), 1);
  EXPECT_EQ(&*ConstBlockAddrRange.begin(), B1);

  // Querying offsets is not affected by address changes.

  BlockOffsetRange = BI->findDataBlocksOnOffset(6);
  ASSERT_EQ(std::distance(BlockOffsetRange.begin(), BlockOffsetRange.end()), 1);
  EXPECT_EQ(&*BlockOffsetRange.begin(), B2);

  ConstBlockOffsetRange = CBI->findDataBlocksOnOffset(6);
  ASSERT_EQ(
      std::distance(ConstBlockOffsetRange.begin(), ConstBlockOffsetRange.end()),
      1);
  EXPECT_EQ(&*ConstBlockOffsetRange.begin(), B2);

  // If the block size changes, queries will be affected.

  B1->setSize(10);

  BlockOffsetRange = BI->findDataBlocksOnOffset(6);
  ASSERT_EQ(std::distance(BlockOffsetRange.begin(), BlockOffsetRange.end()), 2);
  EXPECT_EQ(&*std::next(BlockOffsetRange.begin(), 0), B1);
  EXPECT_EQ(&*std::next(BlockOffsetRange.begin(), 1), B2);

  ConstBlockOffsetRange = CBI->findDataBlocksOnOffset(6);
  ASSERT_EQ(
      std::distance(ConstBlockOffsetRange.begin(), ConstBlockOffsetRange.end()),
      2);
  EXPECT_EQ(&*std::next(ConstBlockOffsetRange.begin(), 0), B1);
  EXPECT_EQ(&*std::next(ConstBlockOffsetRange.begin(), 1), B2);
}

TEST(Unit_ByteInterval, findBlocksAt) {
  auto* BI = ByteInterval::Create(Ctx, 10);
  auto* B1 = BI->addBlock<CodeBlock>(Ctx, 0, 2);
  auto* B2 = BI->addBlock<CodeBlock>(Ctx, 5, 3);
  const ByteInterval* CBI = BI;

  // Methods return empty ranges if ByteInterval has no address.

  auto BlockRange = BI->findBlocksAt(Addr(0));
  EXPECT_TRUE(BlockRange.empty());

  auto ConstBlockRange = CBI->findBlocksAt(Addr(0));
  EXPECT_TRUE(ConstBlockRange.empty());

  BlockRange =
      BI->findBlocksAt(Addr(0), Addr(static_cast<Addr::value_type>(-1)));
  EXPECT_TRUE(BlockRange.empty());

  ConstBlockRange =
      CBI->findBlocksAt(Addr(0), Addr(static_cast<Addr::value_type>(-1)));
  EXPECT_TRUE(BlockRange.empty());

  // Once the address is set, the ranges are no longer empty.

  BI->setAddress(Addr(0));
  BlockRange = BI->findBlocksAt(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 1);
  EXPECT_EQ(&*BlockRange.begin(), B2);

  ConstBlockRange = CBI->findBlocksAt(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 1);
  EXPECT_EQ(&*ConstBlockRange.begin(), B2);

  // Changing the ByteInterval address changes which blocks are found.

  BI->setAddress(Addr(5));
  BlockRange = BI->findBlocksAt(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 1);
  EXPECT_EQ(&*BlockRange.begin(), B1);

  ConstBlockRange = CBI->findBlocksAt(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 1);
  EXPECT_EQ(&*ConstBlockRange.begin(), B1);

  // Method returns empty range if no block starts at address.

  EXPECT_TRUE(BI->findBlocksAt(Addr(0)).empty());
  EXPECT_TRUE(BI->findBlocksAt(Addr(7)).empty());
  EXPECT_TRUE(BI->findBlocksAt(Addr(15)).empty());

  EXPECT_TRUE(CBI->findBlocksAt(Addr(0)).empty());
  EXPECT_TRUE(CBI->findBlocksAt(Addr(7)).empty());
  EXPECT_TRUE(CBI->findBlocksAt(Addr(15)).empty());

  // Querying a range of addreses that start before and/or ends after the
  // ByteInterval is equivalent to querying the ByteInterval's bounds.

  BlockRange = BI->findBlocksAt(Addr(0), Addr(20));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 2);
  EXPECT_EQ(pointers(BlockRange),
            pointers(BI->findBlocksAt(*BI->getAddress(),
                                      *BI->getAddress() + BI->getSize())));
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), B1);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), B2);

  ConstBlockRange = CBI->findBlocksAt(Addr(0), Addr(20));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 2);
  EXPECT_EQ(pointers(ConstBlockRange),
            pointers(CBI->findBlocksAt(*BI->getAddress(),
                                       *BI->getAddress() + BI->getSize())));
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), B1);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), B2);

  // Order matters: querying an empty range produces an empty result.

  BlockRange = BI->findBlocksAt(Addr(20), Addr(0));
  EXPECT_TRUE(BlockRange.empty());

  ConstBlockRange = CBI->findBlocksAt(Addr(20), Addr(0));
  EXPECT_TRUE(ConstBlockRange.empty());
}

TEST(Unit_ByteInterval, findCodeBlocksAt) {
  auto* BI = ByteInterval::Create(Ctx, 10);
  auto* B1 = BI->addBlock<CodeBlock>(Ctx, 0, 2);
  auto* B2 = BI->addBlock<CodeBlock>(Ctx, 5, 3);
  const ByteInterval* CBI = BI;

  // Methods return empty ranges if ByteInterval has no address.

  auto BlockRange = BI->findCodeBlocksAt(Addr(0));
  EXPECT_TRUE(BlockRange.empty());

  auto ConstBlockRange = CBI->findCodeBlocksAt(Addr(0));
  EXPECT_TRUE(ConstBlockRange.empty());

  BlockRange =
      BI->findCodeBlocksAt(Addr(0), Addr(static_cast<Addr::value_type>(-1)));
  EXPECT_TRUE(BlockRange.empty());

  ConstBlockRange =
      CBI->findCodeBlocksAt(Addr(0), Addr(static_cast<Addr::value_type>(-1)));
  EXPECT_TRUE(BlockRange.empty());

  // Once the address is set, the ranges are no longer empty.

  BI->setAddress(Addr(0));
  BlockRange = BI->findCodeBlocksAt(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 1);
  EXPECT_EQ(&*BlockRange.begin(), B2);

  ConstBlockRange = CBI->findCodeBlocksAt(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 1);
  EXPECT_EQ(&*ConstBlockRange.begin(), B2);

  // Changing the ByteInterval address changes which blocks are found.

  BI->setAddress(Addr(5));
  BlockRange = BI->findCodeBlocksAt(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 1);
  EXPECT_EQ(&*BlockRange.begin(), B1);

  ConstBlockRange = CBI->findCodeBlocksAt(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 1);
  EXPECT_EQ(&*ConstBlockRange.begin(), B1);

  // Method returns empty range if no block starts at address.

  EXPECT_TRUE(BI->findCodeBlocksAt(Addr(0)).empty());
  EXPECT_TRUE(BI->findCodeBlocksAt(Addr(7)).empty());
  EXPECT_TRUE(BI->findCodeBlocksAt(Addr(15)).empty());

  EXPECT_TRUE(CBI->findCodeBlocksAt(Addr(0)).empty());
  EXPECT_TRUE(CBI->findCodeBlocksAt(Addr(7)).empty());
  EXPECT_TRUE(CBI->findCodeBlocksAt(Addr(15)).empty());

  // Querying a range of addreses that start before and/or ends after the
  // ByteInterval is equivalent to querying the ByteInterval's bounds.

  BlockRange = BI->findCodeBlocksAt(Addr(0), Addr(20));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 2);
  EXPECT_EQ(pointers(BlockRange),
            pointers(BI->findCodeBlocksAt(*BI->getAddress(),
                                          *BI->getAddress() + BI->getSize())));
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), B1);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), B2);

  ConstBlockRange = CBI->findCodeBlocksAt(Addr(0), Addr(20));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 2);
  EXPECT_EQ(pointers(ConstBlockRange),
            pointers(CBI->findCodeBlocksAt(*BI->getAddress(),
                                           *BI->getAddress() + BI->getSize())));
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), B1);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), B2);

  // Order matters: querying an empty range produces an empty result.

  BlockRange = BI->findCodeBlocksAt(Addr(20), Addr(0));
  EXPECT_TRUE(BlockRange.empty());

  ConstBlockRange = CBI->findCodeBlocksAt(Addr(20), Addr(0));
  EXPECT_TRUE(ConstBlockRange.empty());
}

TEST(Unit_ByteInterval, findDataBlocksAt) {
  auto* BI = ByteInterval::Create(Ctx, 10);
  auto* B1 = BI->addBlock<DataBlock>(Ctx, 0, 2);
  auto* B2 = BI->addBlock<DataBlock>(Ctx, 5, 3);
  const ByteInterval* CBI = BI;

  // Methods return empty ranges if ByteInterval has no address.

  auto BlockRange = BI->findDataBlocksAt(Addr(0));
  EXPECT_TRUE(BlockRange.empty());

  auto ConstBlockRange = CBI->findDataBlocksAt(Addr(0));
  EXPECT_TRUE(ConstBlockRange.empty());

  BlockRange =
      BI->findDataBlocksAt(Addr(0), Addr(static_cast<Addr::value_type>(-1)));
  EXPECT_TRUE(BlockRange.empty());

  ConstBlockRange =
      CBI->findDataBlocksAt(Addr(0), Addr(static_cast<Addr::value_type>(-1)));
  EXPECT_TRUE(BlockRange.empty());

  // Once the address is set, the ranges are no longer empty.

  BI->setAddress(Addr(0));
  BlockRange = BI->findDataBlocksAt(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 1);
  EXPECT_EQ(&*BlockRange.begin(), B2);

  ConstBlockRange = CBI->findDataBlocksAt(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 1);
  EXPECT_EQ(&*ConstBlockRange.begin(), B2);

  // Changing the ByteInterval address changes which blocks are found.

  BI->setAddress(Addr(5));
  BlockRange = BI->findDataBlocksAt(Addr(5));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 1);
  EXPECT_EQ(&*BlockRange.begin(), B1);

  ConstBlockRange = CBI->findDataBlocksAt(Addr(5));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 1);
  EXPECT_EQ(&*ConstBlockRange.begin(), B1);

  // Method returns empty range if no block starts at address.

  EXPECT_TRUE(BI->findDataBlocksAt(Addr(0)).empty());
  EXPECT_TRUE(BI->findDataBlocksAt(Addr(7)).empty());
  EXPECT_TRUE(BI->findDataBlocksAt(Addr(15)).empty());

  EXPECT_TRUE(CBI->findDataBlocksAt(Addr(0)).empty());
  EXPECT_TRUE(CBI->findDataBlocksAt(Addr(7)).empty());
  EXPECT_TRUE(CBI->findDataBlocksAt(Addr(15)).empty());

  // Querying a range of addreses that start before and/or ends after the
  // ByteInterval is equivalent to querying the ByteInterval's bounds.

  BlockRange = BI->findDataBlocksAt(Addr(0), Addr(20));
  ASSERT_EQ(std::distance(BlockRange.begin(), BlockRange.end()), 2);
  EXPECT_EQ(pointers(BlockRange),
            pointers(BI->findDataBlocksAt(*BI->getAddress(),
                                          *BI->getAddress() + BI->getSize())));
  EXPECT_EQ(&*std::next(BlockRange.begin(), 0), B1);
  EXPECT_EQ(&*std::next(BlockRange.begin(), 1), B2);

  ConstBlockRange = CBI->findDataBlocksAt(Addr(0), Addr(20));
  ASSERT_EQ(std::distance(ConstBlockRange.begin(), ConstBlockRange.end()), 2);
  EXPECT_EQ(pointers(ConstBlockRange),
            pointers(CBI->findDataBlocksAt(*BI->getAddress(),
                                           *BI->getAddress() + BI->getSize())));
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 0), B1);
  EXPECT_EQ(&*std::next(ConstBlockRange.begin(), 1), B2);

  // Order matters: querying an empty range produces an empty result.

  BlockRange = BI->findDataBlocksAt(Addr(20), Addr(0));
  EXPECT_TRUE(BlockRange.empty());

  ConstBlockRange = CBI->findDataBlocksAt(Addr(20), Addr(0));
  EXPECT_TRUE(ConstBlockRange.empty());
}
