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
#include <gtirb/ByteInterval.hpp>
#include <gtirb/Context.hpp>
#include <gtirb/Module.hpp>
#include <proto/ByteInterval.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

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
  // Test with fixed address.
  {
    proto::ByteInterval Message;
    {
      Context InnerCtx;
      auto* Original = ByteInterval::Create(InnerCtx, Addr(1), 2);
      Original->toProtobuf(&Message);
    }
    auto* Result = ByteInterval::fromProtobuf(Ctx, nullptr, Message);

    EXPECT_EQ(Result->getAddress(), std::optional<Addr>(1));
    EXPECT_EQ(Result->getSize(), 2);
    EXPECT_EQ(Result->getSection(), nullptr);
  }

  // Test without fixed address.
  {
    proto::ByteInterval Message;
    {
      Context InnerCtx;
      auto* Original = ByteInterval::Create(InnerCtx, std::optional<Addr>(), 2);
      Original->toProtobuf(&Message);
    }
    auto* Result = ByteInterval::fromProtobuf(Ctx, nullptr, Message);

    EXPECT_EQ(Result->getAddress(), std::optional<Addr>());
    EXPECT_EQ(Result->getSize(), 2);
    EXPECT_EQ(Result->getSection(), nullptr);
  }

  // Test with bytes.
  {
    proto::ByteInterval Message;
    {
      Context InnerCtx;
      std::string Contents = "abcd";
      auto* Original = ByteInterval::Create(InnerCtx, std::optional<Addr>(),
                                            Contents.begin(), Contents.end());
      Original->toProtobuf(&Message);
    }
    auto* Result = ByteInterval::fromProtobuf(Ctx, nullptr, Message);

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
    proto::ByteInterval Message;
    {
      Context InnerCtx;
      std::string Contents = "abcd";
      auto* Original =
          ByteInterval::Create(InnerCtx, std::optional<Addr>(),
                               Contents.begin(), Contents.end(), 4, 2);
      Original->toProtobuf(&Message);
    }
    auto* Result = ByteInterval::fromProtobuf(Ctx, nullptr, Message);

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

    proto::ByteInterval Message;
    {
      Context InnerCtx;
      auto* Original = ByteInterval::Create(InnerCtx, Addr(0), 10);
      Original->addBlock(3, CodeBlock::Create(InnerCtx, 1));
      Original->addBlock(6, CodeBlock::Create(InnerCtx, 1));
      Original->addBlock(6, DataBlock::Create(InnerCtx, 1));
      Original->addSymbolicExpression(5, SymAddrConst{8, Sym});
      Original->toProtobuf(&Message);
    }
    auto* Result = ByteInterval::fromProtobuf(Ctx, nullptr, Message);

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
    Result->symbolicExpressionsFromProtobuf(Ctx, Message);
    EXPECT_EQ(std::distance(Result->symbolic_expressions_begin(),
                            Result->symbolic_expressions_end()),
              1);

    EXPECT_EQ(Result->symbolic_expressions_begin()->first, 5);
    EXPECT_TRUE(std::holds_alternative<SymAddrConst>(
        Result->symbolic_expressions_begin()->second));
    EXPECT_EQ(
        std::get<SymAddrConst>(Result->symbolic_expressions_begin()->second)
            .Offset,
        8);
    EXPECT_EQ(
        std::get<SymAddrConst>(Result->symbolic_expressions_begin()->second)
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
    auto NewIt = BI->bytes_begin<uint16_t>(ByteInterval::Endian::little);
    auto OriginalEnd = CompareTo.end();
    auto NewEnd = BI->bytes_end<uint16_t>(ByteInterval::Endian::little);

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
    auto NewIt = BI->bytes_begin<uint16_t>(ByteInterval::Endian::big);
    auto OriginalEnd = CompareTo.end();
    auto NewEnd = BI->bytes_end<uint16_t>(ByteInterval::Endian::big);

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
  auto* M = Module::Create(Ctx);
  auto* S = M->addSection(Section::Create(Ctx, "test"));
  auto* BI = S->addByteInterval(ByteInterval::Create(Ctx, Addr(0), 10));

  auto* B1 = BI->addBlock(0, CodeBlock::Create(Ctx, 1));
  auto* B2 = BI->addBlock(1, DataBlock::Create(Ctx, 1));
  auto* B3 = BI->addBlock(2, CodeBlock::Create(Ctx, 1));
  auto* B4 = BI->addBlock(3, DataBlock::Create(Ctx, 1));

  {
    auto Begin = M->code_blocks_begin(), End = M->code_blocks_end();
    ASSERT_EQ(std::distance(Begin, End), 2);
    ASSERT_NE(std::find_if(Begin, End, [&](const auto& N) { return B1 == &N; }),
              End);
    ASSERT_NE(std::find_if(Begin, End, [&](const auto& N) { return B3 == &N; }),
              End);
  }

  {
    auto Begin = M->data_blocks_begin(), End = M->data_blocks_end();
    ASSERT_EQ(std::distance(Begin, End), 2);
    ASSERT_NE(std::find_if(Begin, End, [&](const auto& N) { return B2 == &N; }),
              End);
    ASSERT_NE(std::find_if(Begin, End, [&](const auto& N) { return B4 == &N; }),
              End);
  }

  BI->removeBlock(B1);

  {
    auto Begin = M->code_blocks_begin(), End = M->code_blocks_end();
    ASSERT_EQ(std::distance(Begin, End), 1);
    ASSERT_NE(std::find_if(Begin, End, [&](const auto& N) { return B3 == &N; }),
              End);
  }

  {
    auto Begin = M->data_blocks_begin(), End = M->data_blocks_end();
    ASSERT_EQ(std::distance(Begin, End), 2);
    ASSERT_NE(std::find_if(Begin, End, [&](const auto& N) { return B2 == &N; }),
              End);
    ASSERT_NE(std::find_if(Begin, End, [&](const auto& N) { return B4 == &N; }),
              End);
  }

  BI->removeBlock(B2);

  {
    auto Begin = M->code_blocks_begin(), End = M->code_blocks_end();
    ASSERT_EQ(std::distance(Begin, End), 1);
    ASSERT_NE(std::find_if(Begin, End, [&](const auto& N) { return B3 == &N; }),
              End);
  }

  {
    auto Begin = M->data_blocks_begin(), End = M->data_blocks_end();
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
}
