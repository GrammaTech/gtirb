//===- ByteInterval.cpp -----------------------------------------*- C++ -*-===//
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
#include "Serialization.hpp"
#include "SymbolicExpressionSerialization.hpp"
#include <gtirb/ByteInterval.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtirb/proto/ByteInterval.pb.h>
#include <iterator>

using namespace gtirb;

class ByteInterval::CodeBlockObserverImpl : public CodeBlockObserver {
public:
  CodeBlockObserverImpl(ByteInterval* BI_) : BI(BI_) {}

  ChangeStatus sizeChange(CodeBlock* B, uint64_t OldSize,
                          uint64_t NewSize) override;

private:
  ByteInterval* BI;
};

class ByteInterval::DataBlockObserverImpl : public DataBlockObserver {
public:
  DataBlockObserverImpl(ByteInterval* BI_) : BI(BI_) {}

  ChangeStatus sizeChange(DataBlock* B, uint64_t OldSize,
                          uint64_t NewSize) override;

private:
  ByteInterval* BI;
};

ByteInterval::ByteInterval(Context& C) : ByteInterval(C, std::nullopt, 0, 0) {}

ByteInterval::ByteInterval(Context& C, std::optional<Addr> A, uint64_t S,
                           uint64_t InitSize)
    : Node(C, Kind::ByteInterval), Address(A), Size(S), Bytes(InitSize),
      CBO(std::make_unique<CodeBlockObserverImpl>(this)),
      DBO(std::make_unique<DataBlockObserverImpl>(this)) {}

void ByteInterval::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());

  if (Address.has_value()) {
    Message->set_has_address(true);
    Message->set_address((uint64_t)*Address);
  } else {
    Message->set_has_address(false);
  }

  Message->set_size(getSize());
  auto BytesIt = bytes_begin<char>();
  auto InitSize = getInitializedSize();
  Message->mutable_contents()->reserve(InitSize);
  std::copy(BytesIt, BytesIt + InitSize,
            std::back_inserter(*Message->mutable_contents()));

  for (const auto& N : this->blocks()) {
    auto* ProtoBlock = Message->add_blocks();

    switch (N.getKind()) {
    case Node::Kind::CodeBlock: {
      auto& B = cast<CodeBlock>(N);
      ProtoBlock->set_offset(B.getOffset());
      B.toProtobuf(ProtoBlock->mutable_code());
    } break;
    case Node::Kind::DataBlock: {
      auto& B = cast<DataBlock>(N);
      ProtoBlock->set_offset(B.getOffset());
      B.toProtobuf(ProtoBlock->mutable_data());
    } break;
    default: {
      assert(!"unknown Node::Kind in ByteInterval::toProtobuf");
    }
    }
  }

  auto& ProtoSymExpr = *Message->mutable_symbolic_expressions();
  for (const auto& SEE : this->symbolic_expressions()) {
    ProtoSymExpr[SEE.getOffset()] =
        gtirb::toProtobuf(SEE.getSymbolicExpression());
  }
}

ByteInterval* ByteInterval::fromProtobuf(Context& C,
                                         const MessageType& Message) {
  std::optional<Addr> A;
  if (Message.has_address()) {
    A = Addr(Message.address());
  }

  ByteInterval* BI = ByteInterval::Create(
      C, A, Message.contents().begin(), Message.contents().end(),
      Message.size(), Message.contents().size());

  if (!setNodeUUIDFromBytes(BI, Message.uuid()))
    return nullptr;

  for (const auto& ProtoBlock : Message.blocks()) {
    switch (ProtoBlock.value_case()) {
    case proto::Block::ValueCase::kCode: {
      auto* B = CodeBlock::fromProtobuf(C, ProtoBlock.code());
      if (!B)
        return nullptr;
      BI->addBlock(ProtoBlock.offset(), B);
    } break;
    case proto::Block::ValueCase::kData: {
      auto* B = DataBlock::fromProtobuf(C, ProtoBlock.data());
      if (!B)
        return nullptr;
      BI->addBlock(ProtoBlock.offset(), B);
    } break;
    default: {
      assert(!"unknown Block::ValueCase in ByteInterval::fromProtobuf");
      return nullptr;
    }
    }
  }
  return BI;
}

bool ByteInterval::symbolicExpressionsFromProtobuf(Context& C,
                                                   const MessageType& Message) {
  bool Result = true;
  for (const auto& Pair : Message.symbolic_expressions()) {
    SymbolicExpression SymExpr;
    if (gtirb::fromProtobuf(C, SymExpr, Pair.second))
      SymbolicExpressions[Pair.first] = SymExpr;
    else {
      Result = false;
      break;
    }
  }
  return Result;
}

// Present for testing purposes only.
void ByteInterval::save(std::ostream& Out) const {
  MessageType Message;
  this->toProtobuf(&Message);
  Message.SerializeToOstream(&Out);
}

// Present for testing purposes only.
ByteInterval* ByteInterval::load(Context& C, std::istream& In) {
  MessageType Message;
  Message.ParseFromIstream(&In);
  auto BI = ByteInterval::fromProtobuf(C, Message);
  return BI;
}

// Present for testing purposes only.
bool ByteInterval::loadSymbolicExpressions(Context& C, std::istream& In) {
  MessageType Message;
  Message.ParseFromIstream(&In);
  return ByteInterval::symbolicExpressionsFromProtobuf(C, Message);
}

void ByteInterval::setAddress(std::optional<Addr> A) {
  if (Observer) {
    std::optional<AddrRange> OldExtent = addressRange(*this);
    Address = A;
    [[maybe_unused]] ChangeStatus Status =
        Observer->changeExtent(this, OldExtent, addressRange(*this));
    assert(Status != ChangeStatus::Rejected &&
           "recovering from rejected address change is not implemented yet");
    Status = Observer->moveCodeBlocks(this, code_blocks());
    assert(Status != ChangeStatus::Rejected &&
           "recovering from rejected address change is not implemented yet");
    Status = Observer->moveDataBlocks(this, data_blocks());
    assert(Status != ChangeStatus::Rejected &&
           "recovering from rejected address change is not implemented yet");
  } else {
    Address = A;
  }
}

void ByteInterval::setSize(uint64_t S) {
  if (Observer) {
    std::optional<AddrRange> OldExtent = addressRange(*this);
    Size = S;
    [[maybe_unused]] ChangeStatus Status =
        Observer->changeExtent(this, OldExtent, addressRange(*this));
    assert(Status != ChangeStatus::Rejected &&
           "recovering from rejected size change is not implemented yet");
  } else {
    Size = S;
  }
  if (S < getInitializedSize()) {
    setInitializedSize(S);
  }
}

static inline ChangeStatus removeBlocks(ByteIntervalObserver* Observer,
                                        ByteInterval* BI,
                                        ByteInterval::code_block_range Range) {
  return Observer->removeCodeBlocks(BI, Range);
}

static inline ChangeStatus removeBlocks(ByteIntervalObserver* Observer,
                                        ByteInterval* BI,
                                        ByteInterval::data_block_range Range) {
  return Observer->removeDataBlocks(BI, Range);
}

template <typename BlockType, typename IterType>
ChangeStatus ByteInterval::removeBlock(BlockType* B) {
  auto& Index = Blocks.get<by_pointer>();
  if (auto Iter = Index.find(B); Iter != Index.end()) {
    if (Observer) {
      auto Begin = Blocks.project<0>(Iter);
      auto End = std::next(Begin);
      auto Range = boost::make_iterator_range(
          IterType(typename IterType::base_type(Begin, End)),
          IterType(typename IterType::base_type(End, End)));
      [[maybe_unused]] ChangeStatus Status =
          removeBlocks(Observer, this, Range);
      // None of the known observers reject removals. If that changes, this
      // implementation will need to be changed as well. Because addBlock
      // assumes that this removal will not be rejected, it will also need to
      // be updated.
      assert(Status != ChangeStatus::Rejected &&
             "recovering from rejected removal is not implemented yet");
    }

    [[maybe_unused]] ChangeStatus Status = sizeChange(B, B->getSize(), 0);
    assert(Status != ChangeStatus::Rejected &&
           "recovering from rejected removal is not implemented yet");
    Index.erase(Iter);
    B->setParent(nullptr, nullptr);
    return ChangeStatus::Accepted;
  }
  return ChangeStatus::NoChange;
}

ChangeStatus ByteInterval::removeBlock(CodeBlock* B) {
  return removeBlock<CodeBlock, code_block_iterator>(B);
}

ChangeStatus ByteInterval::removeBlock(DataBlock* B) {
  return removeBlock<DataBlock, data_block_iterator>(B);
}

static inline CodeBlockObserver* getObserver(CodeBlock*, CodeBlockObserver* CBO,
                                             DataBlockObserver*) {
  return CBO;
}

static inline DataBlockObserver* getObserver(DataBlock*, CodeBlockObserver*,
                                             DataBlockObserver* DBO) {
  return DBO;
}

static inline ChangeStatus addBlocks(ByteIntervalObserver* Observer,
                                     ByteInterval* BI,
                                     ByteInterval::code_block_range Range) {
  return Observer->addCodeBlocks(BI, Range);
}

static inline ChangeStatus addBlocks(ByteIntervalObserver* Observer,
                                     ByteInterval* BI,
                                     ByteInterval::data_block_range Range) {
  return Observer->addDataBlocks(BI, Range);
}

static inline ChangeStatus moveBlocks(ByteIntervalObserver* Observer,
                                      ByteInterval* BI,
                                      ByteInterval::code_block_range Range) {
  return Observer->moveCodeBlocks(BI, Range);
}

static inline ChangeStatus moveBlocks(ByteIntervalObserver* Observer,
                                      ByteInterval* BI,
                                      ByteInterval::data_block_range Range) {
  return Observer->moveDataBlocks(BI, Range);
}

template <typename BlockType, typename IterType>
ChangeStatus ByteInterval::addBlock(uint64_t Off, BlockType* B) {
  // Determine if we're moving or adding a block.
  bool IsMove = false;
  ByteInterval* BI = B->getByteInterval();
  if (BI == this) {
    if (Off == B->getOffset()) {
      return ChangeStatus::NoChange;
    } else {
      IsMove = true;
    }
  }

  // Remove the old block.
  if (!IsMove) {
    if (BI) {
      [[maybe_unused]] ChangeStatus Status = BI->removeBlock(B);
      assert(Status != ChangeStatus::Rejected &&
             "failed to remove node from parent");
    }

    B->setParent(this, getObserver(B, CBO.get(), DBO.get()));
  }

  // Update our own indices, part 1.
  if (IsMove) {
    [[maybe_unused]] ChangeStatus ResizeStatus = sizeChange(B, B->getSize(), 0);
    assert(ResizeStatus != ChangeStatus::Rejected &&
           "recovering from rejected size change is unimplemented");
  }

  // Actually modify the offset.
  auto Begin = Blocks.get<by_offset>().end();
  if (IsMove) {
    Blocks.get<by_pointer>().modify(Blocks.get<by_pointer>().find(B),
                                    [Off](auto& Entry) { Entry.Offset = Off; });
    Begin = Blocks.project<by_offset>(Blocks.get<by_pointer>().find(B));
  } else {
    Begin = Blocks.emplace(Off, B).first;
  }

  // Update our own indices, part 2.
  [[maybe_unused]] ChangeStatus ResizeStatus = sizeChange(B, 0, B->getSize());
  assert(ResizeStatus != ChangeStatus::Rejected &&
         "recovering from rejected size change is unimplemented");

  // Only fire events if we have an observer.
  if (!Observer) {
    return ChangeStatus::Accepted;
  }

  // Get the range to use.
  assert(Begin != Blocks.get<by_offset>().end());
  auto End = std::next(Begin);
  auto Range = boost::make_iterator_range(
      IterType(typename IterType::base_type(Begin, End)),
      IterType(typename IterType::base_type(End, End)));

  // Fire the move/add event.
  [[maybe_unused]] ChangeStatus Status;
  if (IsMove) {
    Status = moveBlocks(Observer, this, Range);
  } else {
    Status = addBlocks(Observer, this, Range);
  }

  // None of the known observers reject insertions. If that changes, this
  // implementation must be updated.
  assert(Status != ChangeStatus::Rejected &&
         "recovering from rejected insertion is unimplemented");

  // All good.
  return ChangeStatus::Accepted;
}

ChangeStatus ByteInterval::addBlock(uint64_t Off, CodeBlock* B) {
  return addBlock<CodeBlock, code_block_iterator>(Off, B);
}

ChangeStatus ByteInterval::addBlock(uint64_t Off, DataBlock* B) {
  return addBlock<DataBlock, data_block_iterator>(Off, B);
}

ChangeStatus ByteInterval::CodeBlockObserverImpl::sizeChange(CodeBlock* B,
                                                             uint64_t OldSize,
                                                             uint64_t NewSize) {
  return BI->sizeChange(B, OldSize, NewSize);
}

ChangeStatus ByteInterval::DataBlockObserverImpl::sizeChange(DataBlock* B,
                                                             uint64_t OldSize,
                                                             uint64_t NewSize) {
  return BI->sizeChange(B, OldSize, NewSize);
}

ChangeStatus ByteInterval::sizeChange(Node* N, uint64_t OldSize,
                                      uint64_t NewSize) {
  auto& Index = Blocks.get<by_pointer>();
  auto Iter = Index.find(N);
  assert(Iter != Index.end() && "block observed by non-owner");
  BlockOffsets.subtract(
      std::make_pair(ByteInterval::BlockIntMap::interval_type::right_open(
                         Iter->Offset, Iter->Offset + OldSize),
                     ByteInterval::BlockIntMap::codomain_type({&*Iter})));
  BlockOffsets.add(
      std::make_pair(ByteInterval::BlockIntMap::interval_type::right_open(
                         Iter->Offset, Iter->Offset + NewSize),
                     ByteInterval::BlockIntMap::codomain_type({&*Iter})));
  return ChangeStatus::Accepted;
}
