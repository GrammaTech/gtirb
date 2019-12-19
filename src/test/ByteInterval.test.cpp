//===- Block.test.cpp -------------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2018 GrammaTech, Inc.
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
#include <proto/ByteInterval.pb.h>
#include <gtest/gtest.h>

using namespace gtirb;

static Context Ctx;

TEST(Unit_ByteInterval, ctor) {
  EXPECT_NE(ByteInterval::Create(Ctx, nullptr, Addr(1), 100), nullptr);
}

TEST(Unit_ByteInterval, gettersSetters) {
  auto BI = ByteInterval::Create(Ctx, nullptr, std::optional<Addr>(), 2);
  EXPECT_EQ(std::optional<Addr>(), BI->getAddress());
  EXPECT_EQ(2, BI->getSize());
  EXPECT_EQ(2, BI->getAllocatedSize());
  EXPECT_EQ(BI->getSection(), nullptr);

  BI->setAddress(Addr(1));
  EXPECT_EQ(std::optional<Addr>(1), BI->getAddress());
  EXPECT_EQ(2, BI->getSize());
  EXPECT_EQ(2, BI->getAllocatedSize());
  EXPECT_EQ(BI->getSection(), nullptr);

  BI->setSize(10);
  EXPECT_EQ(std::optional<Addr>(1), BI->getAddress());
  EXPECT_EQ(10, BI->getSize());
  EXPECT_EQ(2, BI->getAllocatedSize());
  EXPECT_EQ(BI->getSection(), nullptr);

  BI->setAllocatedSize(5);
  EXPECT_EQ(std::optional<Addr>(1), BI->getAddress());
  EXPECT_EQ(10, BI->getSize());
  EXPECT_EQ(5, BI->getAllocatedSize());
  EXPECT_EQ(BI->getSection(), nullptr);

  BI->setSize(1);
  EXPECT_EQ(std::optional<Addr>(1), BI->getAddress());
  EXPECT_EQ(1, BI->getSize());
  EXPECT_EQ(1, BI->getAllocatedSize());
  EXPECT_EQ(BI->getSection(), nullptr);

  BI->setAllocatedSize(20);
  EXPECT_EQ(std::optional<Addr>(1), BI->getAddress());
  EXPECT_EQ(20, BI->getSize());
  EXPECT_EQ(20, BI->getAllocatedSize());
  EXPECT_EQ(BI->getSection(), nullptr);
}

TEST(Unit_ByteInterval, protobufRoundTrip) {
  // Test with fixed address.
  {
    proto::ByteInterval Message;
    {
      Context InnerCtx;
      ByteInterval* Original =
          ByteInterval::Create(InnerCtx, nullptr, Addr(1), 2);
      Original->toProtobuf(&Message);
    }
    ByteInterval* Result = ByteInterval::fromProtobuf(Ctx, nullptr, Message);

    EXPECT_EQ(Result->getAddress(), std::optional<Addr>(1));
    EXPECT_EQ(Result->getSize(), 2);
    EXPECT_EQ(Result->getSection(), nullptr);
  }

  // Test without fixed address.
  {
    proto::ByteInterval Message;
    {
      Context InnerCtx;
      ByteInterval* Original =
          ByteInterval::Create(InnerCtx, nullptr, std::optional<Addr>(), 2);
      Original->toProtobuf(&Message);
    }
    ByteInterval* Result = ByteInterval::fromProtobuf(Ctx, nullptr, Message);

    EXPECT_EQ(Result->getAddress(), std::optional<Addr>());
    EXPECT_EQ(Result->getSize(), 2);
    EXPECT_EQ(Result->getSection(), nullptr);
  }

  // Test with bytes.
  {
    proto::ByteInterval Message;
    {
      Context InnerCtx;
      std::string contents = "abcd";
      ByteInterval* Original =
          ByteInterval::Create(InnerCtx, nullptr, std::optional<Addr>(),
                               contents.begin(), contents.end());
      Original->toProtobuf(&Message);
    }
    ByteInterval* Result = ByteInterval::fromProtobuf(Ctx, nullptr, Message);

    EXPECT_EQ(Result->getAddress(), std::optional<Addr>());
    EXPECT_EQ(Result->getSize(), 4);
    EXPECT_EQ(Result->getAllocatedSize(), 4);
    EXPECT_EQ(Result->getSection(), nullptr);

    std::string resultBytes;
    std::copy(Result->bytes_begin<char>(), Result->bytes_end<char>(),
              std::back_inserter(resultBytes));
    ASSERT_EQ(resultBytes, "abcd");
  }

  // Test truncating of unallocated bytes.
  {
    proto::ByteInterval Message;
    {
      Context InnerCtx;
      std::string contents = "abcd";
      ByteInterval* Original =
          ByteInterval::Create(InnerCtx, nullptr, std::optional<Addr>(),
                               contents.begin(), contents.end(), 4, 2);
      Original->toProtobuf(&Message);
    }
    ByteInterval* Result = ByteInterval::fromProtobuf(Ctx, nullptr, Message);

    EXPECT_EQ(Result->getAddress(), std::optional<Addr>());
    EXPECT_EQ(Result->getSize(), 4);
    EXPECT_EQ(Result->getAllocatedSize(), 2);
    EXPECT_EQ(Result->getSection(), nullptr);

    std::string resultBytes;
    std::copy(Result->bytes_begin<char>(), Result->bytes_end<char>(),
              std::back_inserter(resultBytes));
    // Explode the string comparison, because the default one for std::string
    // does not like embedded NULs whatsoever.
    ASSERT_EQ(resultBytes.size(), 4);
    ASSERT_EQ(resultBytes[0], 'a');
    ASSERT_EQ(resultBytes[1], 'b');
    ASSERT_EQ(resultBytes[2], '\0');
    ASSERT_EQ(resultBytes[3], '\0');
  }

  // Test with subobjects.
  {
    auto Sym = Symbol::Create(Ctx, nullptr, "test");

    proto::ByteInterval Message;
    {
      Context InnerCtx;
      ByteInterval* Original =
          ByteInterval::Create(InnerCtx, nullptr, Addr(0), 10);
      Original->addCodeBlock(InnerCtx, 3, 1);
      Original->addCodeBlock(InnerCtx, 6, 1);
      Original->addDataBlock(InnerCtx, 6, 1);
      Original->addSymbolicExpression<SymAddrConst>(5, 8, Sym);
      Original->toProtobuf(&Message);
    }
    ByteInterval* Result = ByteInterval::fromProtobuf(Ctx, nullptr, Message);

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

    EXPECT_EQ(Result->blocks_begin()->getOffset(), 3);
    EXPECT_EQ(std::next(Result->blocks_begin())->getOffset(), 6);
    EXPECT_EQ(std::next(std::next(Result->blocks_begin()))->getOffset(), 6);

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
  std::string contents = "hello, world!";

  // Test all allocated bytes.
  {
    auto BI = ByteInterval::Create(Ctx, nullptr, std::optional<Addr>(),
                                   contents.begin(), contents.end());
    EXPECT_EQ(BI->getSize(), contents.size());

    auto originalIt = contents.begin();
    auto newIt = BI->bytes_begin<char>();
    auto originalEnd = contents.end();
    auto newEnd = BI->bytes_end<char>();

    while (originalIt != originalEnd && newIt != newEnd) {
      EXPECT_EQ(*originalIt, *newIt);
      ++originalIt;
      ++newIt;
    }
    EXPECT_EQ(originalIt, originalEnd);
    EXPECT_EQ(newIt, newEnd);
  }

  // Test some unallocated bytes.
  {
    auto BI = ByteInterval::Create(Ctx, nullptr, std::optional<Addr>(),
                                   contents.begin(), contents.end(), 100);
    EXPECT_EQ(BI->getSize(), 100);

    auto originalIt = contents.begin();
    auto newIt = BI->bytes_begin<char>();
    auto originalEnd = contents.end();
    auto newEnd = BI->bytes_end<char>();

    while (originalIt != originalEnd && newIt != newEnd) {
      EXPECT_EQ(*originalIt, *newIt);
      ++originalIt;
      ++newIt;
    }
    EXPECT_EQ(originalIt, originalEnd);
    EXPECT_NE(newIt, newEnd);

    while (newIt != newEnd) {
      EXPECT_EQ(*newIt, '\0');
      ++newIt;
    }
    EXPECT_EQ(std::distance(BI->bytes_begin<char>(), newIt), 100);
  }
}

template <typename T> static T str2(const char* s) {
  // TODO: is an endian conversion needed here?
  return *(const T*)s;
}

TEST(Unit_ByteInterval, byteVectorInts) {
  std::string contents = "hello, world!!??";
  auto BI = ByteInterval::Create(Ctx, nullptr, std::optional<Addr>(),
                                 contents.begin(), contents.end());
  EXPECT_EQ(contents.size(), 16);

  // 16 bits
  {
    std::vector<uint16_t> compareTo = {
        str2<uint16_t>("he"), str2<uint16_t>("ll"), str2<uint16_t>("o,"),
        str2<uint16_t>(" w"), str2<uint16_t>("or"), str2<uint16_t>("ld"),
        str2<uint16_t>("!!"), str2<uint16_t>("??")};

    auto originalIt = compareTo.begin();
    auto newIt = BI->bytes_begin<uint16_t>();
    auto originalEnd = compareTo.end();
    auto newEnd = BI->bytes_end<uint16_t>();

    EXPECT_EQ(std::distance(newIt, newEnd), 8);
    while (originalIt != originalEnd && newIt != newEnd) {
      EXPECT_EQ(*originalIt, *newIt);
      ++originalIt;
      ++newIt;
    }
    EXPECT_EQ(originalIt, originalEnd);
    EXPECT_EQ(newIt, newEnd);
  }

  // 32 bits
  {
    std::vector<uint32_t> compareTo = {
        str2<uint32_t>("hell"), str2<uint32_t>("o, w"), str2<uint32_t>("orld"),
        str2<uint32_t>("!!??")};

    auto originalIt = compareTo.begin();
    auto newIt = BI->bytes_begin<uint32_t>();
    auto originalEnd = compareTo.end();
    auto newEnd = BI->bytes_end<uint32_t>();

    EXPECT_EQ(std::distance(newIt, newEnd), 4);
    while (originalIt != originalEnd && newIt != newEnd) {
      EXPECT_EQ(*originalIt, *newIt);
      ++originalIt;
      ++newIt;
    }
    EXPECT_EQ(originalIt, originalEnd);
    EXPECT_EQ(newIt, newEnd);
  }

  // 64 bits
  {
    std::vector<uint64_t> compareTo = {str2<uint64_t>("hello, w"),
                                       str2<uint64_t>("orld!!??")};

    auto originalIt = compareTo.begin();
    auto newIt = BI->bytes_begin<uint64_t>();
    auto originalEnd = compareTo.end();
    auto newEnd = BI->bytes_end<uint64_t>();

    EXPECT_EQ(std::distance(newIt, newEnd), 2);
    while (originalIt != originalEnd && newIt != newEnd) {
      EXPECT_EQ(*originalIt, *newIt);
      ++originalIt;
      ++newIt;
    }
    EXPECT_EQ(originalIt, originalEnd);
    EXPECT_EQ(newIt, newEnd);
  }
}

TEST(Unit_ByteInterval, byteVectorEndian) {
  std::string contents = "hello, world!!??";
  auto BI = ByteInterval::Create(Ctx, nullptr, std::optional<Addr>(),
                                 contents.begin(), contents.end());
  EXPECT_EQ(contents.size(), 16);

  // Test using little endian.
  {
    std::vector<uint16_t> compareTo = {
        str2<uint16_t>("he"), str2<uint16_t>("ll"), str2<uint16_t>("o,"),
        str2<uint16_t>(" w"), str2<uint16_t>("or"), str2<uint16_t>("ld"),
        str2<uint16_t>("!!"), str2<uint16_t>("??")};

    auto originalIt = compareTo.begin();
    auto newIt = BI->bytes_begin<uint16_t>(ByteInterval::Endian::little);
    auto originalEnd = compareTo.end();
    auto newEnd = BI->bytes_end<uint16_t>(ByteInterval::Endian::little);

    EXPECT_EQ(std::distance(newIt, newEnd), 8);
    while (originalIt != originalEnd && newIt != newEnd) {
      EXPECT_EQ(*originalIt, *newIt);
      ++originalIt;
      ++newIt;
    }
    EXPECT_EQ(originalIt, originalEnd);
    EXPECT_EQ(newIt, newEnd);
  }

  // Test using big endian.
  {
    std::vector<uint16_t> compareTo = {
        str2<uint16_t>("eh"), str2<uint16_t>("ll"), str2<uint16_t>(",o"),
        str2<uint16_t>("w "), str2<uint16_t>("ro"), str2<uint16_t>("dl"),
        str2<uint16_t>("!!"), str2<uint16_t>("??")};

    auto originalIt = compareTo.begin();
    auto newIt = BI->bytes_begin<uint16_t>(ByteInterval::Endian::big);
    auto originalEnd = compareTo.end();
    auto newEnd = BI->bytes_end<uint16_t>(ByteInterval::Endian::big);

    EXPECT_EQ(std::distance(newIt, newEnd), 8);
    while (originalIt != originalEnd && newIt != newEnd) {
      EXPECT_EQ(*originalIt, *newIt);
      ++originalIt;
      ++newIt;
    }
    EXPECT_EQ(originalIt, originalEnd);
    EXPECT_EQ(newIt, newEnd);
  }
}

TEST(Unit_ByteInterval, byteVectorInsert) {
  std::string contents = "0123456789";
  auto BI = ByteInterval::Create(Ctx, nullptr, std::optional<Addr>(),
                                 contents.begin(), contents.end());
  {
    std::string toInsert = "abcd";
    BI->insertBytes<char>(BI->bytes_begin<char>(), toInsert.begin(),
                          toInsert.end());
    std::string result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(result));
    ASSERT_EQ(result, "abcd0123456789");
  }

  {
    std::string toInsert = "efg";
    BI->insertBytes<char>(BI->bytes_begin<char>() + 6, toInsert.begin(),
                          toInsert.end());
    std::string result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(result));
    ASSERT_EQ(result, "abcd01efg23456789");
  }

  {
    std::string toInsert = "hi";
    BI->insertBytes<char>(BI->bytes_end<char>(), toInsert.begin(),
                          toInsert.end());
    std::string result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(result));
    ASSERT_EQ(result, "abcd01efg23456789hi");
  }

  // Test with larger, non-char values.
  {
    std::vector<uint32_t> toInsert = {str2<uint32_t>("(hel"),
                                      str2<uint32_t>("lo.)")};
    BI->insertBytes<uint32_t>(BI->bytes_begin<uint32_t>() + 1, toInsert.begin(),
                              toInsert.end());
    std::string result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(result));
    ASSERT_EQ(result, "abcd(hello.)01efg23456789hi");
  }
}

TEST(Unit_ByteInterval, byteVectorErase) {
  std::string contents = "0123456789";
  auto BI = ByteInterval::Create(Ctx, nullptr, std::optional<Addr>(),
                                 contents.begin(), contents.end());

  {
    BI->eraseBytes<char>(BI->bytes_begin<char>(), BI->bytes_begin<char>() + 2);
    std::string result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(result));
    ASSERT_EQ(result, "23456789");
  }

  {
    BI->eraseBytes<char>(BI->bytes_begin<char>() + 4,
                         BI->bytes_begin<char>() + 5);
    std::string result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(result));
    ASSERT_EQ(result, "2345789");
  }

  {
    BI->eraseBytes<char>(BI->bytes_begin<char>() + 6, BI->bytes_end<char>());
    std::string result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(result));
    ASSERT_EQ(result, "234578");
  }

  // Test larger value types.
  {
    BI->eraseBytes<uint16_t>(BI->bytes_begin<uint16_t>() + 1,
                             BI->bytes_begin<uint16_t>() + 2);
    std::string result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(result));
    ASSERT_EQ(result, "2378");
  }

  // Test clearing the bytes.
  {
    BI->eraseBytes<uint32_t>(BI->bytes_begin<uint32_t>(),
                             BI->bytes_end<uint32_t>());
    std::string result;
    std::copy(BI->bytes_begin<char>(), BI->bytes_end<char>(),
              std::back_inserter(result));
    ASSERT_EQ(result, "");
  }
}
