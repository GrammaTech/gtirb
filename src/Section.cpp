//===- Section.cpp ----------------------------------------------*- C++ -*-===//
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
#include "Section.hpp"
#include "IR.hpp"
#include "Serialization.hpp"

using namespace gtirb;

class Section::ByteIntervalObserverImpl : public ByteIntervalObserver {
public:
  ByteIntervalObserverImpl(Section* S_) : S(S_) {}

  ChangeStatus addCodeBlocks(ByteInterval* BI,
                             ByteInterval::code_block_range Blocks) override;

  ChangeStatus moveCodeBlocks(ByteInterval* BI,
                              ByteInterval::code_block_range Blocks) override;

  ChangeStatus removeCodeBlocks(ByteInterval* BI,
                                ByteInterval::code_block_range Blocks) override;

  ChangeStatus addDataBlocks(ByteInterval* BI,
                             ByteInterval::data_block_range Blocks) override;

  ChangeStatus moveDataBlocks(ByteInterval* BI,
                              ByteInterval::data_block_range Blocks) override;

  ChangeStatus removeDataBlocks(ByteInterval* BI,
                                ByteInterval::data_block_range Blocks) override;

  ChangeStatus
  changeExtent(ByteInterval* BI,
               std::function<void(ByteInterval*)> Callback) override;

private:
  Section* S;
};

Section::Section(Context& C) : Section(C, std::string{}) {}

Section::Section(Context& C, const std::string& N)
    : Node(C, Kind::Section), Name(N),
      BIO(std::make_unique<ByteIntervalObserverImpl>(this)) {}

Section::Section(Context& C, const std::string& N, const UUID& U)
    : Node(C, Kind::Section, U), Name(N),
      BIO(std::make_unique<ByteIntervalObserverImpl>(this)) {}

bool Section::operator==(const Section& Other) const {
  return this->getAddress() == Other.getAddress() &&
         this->getSize() == Other.getSize() && this->Name == Other.Name;
}

bool Section::operator!=(const Section& Other) const {
  return !(*this == Other);
}

void Section::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_name(this->Name);
  for (auto Flag : flags()) {
    Message->add_section_flags(static_cast<proto::SectionFlag>(Flag));
  }
  for (const auto& Interval : byte_intervals()) {
    Interval.toProtobuf(Message->add_byte_intervals());
  }
}

ErrorOr<Section*> Section::fromProtobuf(Context& C,
                                        const MessageType& Message) {
  UUID Id;
  if (!uuidFromBytes(Message.uuid(), Id))
    return {IR::load_error::BadUUID, "Could not load section"};

  auto* S = Section::Create(C, Message.name(), Id);
  for (int I = 0, E = Message.section_flags_size(); I != E; ++I) {
    S->addFlag(static_cast<SectionFlag>(Message.section_flags(I)));
  }
  for (const auto& ProtoInterval : Message.byte_intervals()) {
    auto BI = ByteInterval::fromProtobuf(C, ProtoInterval);
    if (!BI) {
      ErrorInfo err{IR::load_error::CorruptSection,
                    "Could not load section" + Message.name()};
      return joinErrors(err, BI.getError());
    }
    S->addByteInterval(*BI);
  }
  return S;
}

// Present for testing purposes only.
void Section::save(std::ostream& Out) const {
  MessageType Message;
  this->toProtobuf(&Message);
  Message.SerializeToOstream(&Out);
}

// Present for testing purposes only.
Section* Section::load(Context& C, std::istream& In) {
  MessageType Message;
  Message.ParseFromIstream(&In);
  auto S = Section::fromProtobuf(C, Message);
  if (S)
    return *S;
  return nullptr;
}

ChangeStatus Section::removeByteInterval(ByteInterval* BI) {
  auto& Index = ByteIntervals.get<by_pointer>();
  if (auto Iter = Index.find(BI); Iter != Index.end()) {
    if (Observer) {
      auto Begin = ByteIntervals.project<by_address>(Iter);
      auto End = std::next(Begin);
      [[maybe_unused]] ChangeStatus Status =
          Observer->removeCodeBlocks(this, makeCodeBlockRange(Begin, End));
      // None of the known observers reject removals. If that changes, this
      // implementation will need to be changed as well. Because
      // addByteInterval assumes that removal will not be rejected, it will
      // need to be updated.
      assert(Status != ChangeStatus::Rejected &&
             "recovering from rejected removal is not implemented yet");
    }

    removeByteIntervalAddrs(BI);
    Index.erase(Iter);
    BI->setParent(nullptr, nullptr);
    [[maybe_unused]] ChangeStatus Status = updateExtent();
    assert(Status != ChangeStatus::Rejected &&
           "failed to change Section extent after removing ByteInterval");
    return ChangeStatus::Accepted;
  }
  return ChangeStatus::NoChange;
}

ChangeStatus Section::addByteInterval(ByteInterval* BI) {
  if (Section* S = BI->getSection()) {
    if (S == this) {
      return ChangeStatus::NoChange;
    }
    [[maybe_unused]] ChangeStatus Status = S->removeByteInterval(BI);
    assert(Status != ChangeStatus::Rejected &&
           "failed to remove node from parent");
  }

  BI->setParent(this, BIO.get());
  auto P = ByteIntervals.emplace(BI);
  if (P.second && Observer) {
    auto Blocks = makeCodeBlockRange(P.first, std::next(P.first));
    [[maybe_unused]] ChangeStatus Status =
        Observer->addCodeBlocks(this, Blocks);
    // None of the known observers reject insertions. If that changes, this
    // implementation must be updated.
    assert(Status != ChangeStatus::Rejected &&
           "recovering from rejected insertion is unimplemented");
  }

  insertByteIntervalAddrs(BI);
  [[maybe_unused]] ChangeStatus Status = updateExtent();
  assert(Status != ChangeStatus::Rejected &&
         "failed to change Section extent after adding ByteInterval");
  return ChangeStatus::Accepted;
}

void Section::removeByteIntervalAddrs(ByteInterval* BI) {
  if (std::optional<AddrRange> OldExtent = addressRange(*BI)) {
    ByteIntervalAddrs.subtract(
        std::make_pair(ByteIntervalIntMap::interval_type::right_open(
                           OldExtent->lower(), OldExtent->upper()),
                       ByteIntervalIntMap::codomain_type({BI})));
  }
}

void Section::insertByteIntervalAddrs(ByteInterval* BI) {
  if (std::optional<AddrRange> NewExtent = addressRange(*BI)) {
    ByteIntervalAddrs.add(
        std::make_pair(ByteIntervalIntMap::interval_type::right_open(
                           NewExtent->lower(), NewExtent->upper()),
                       ByteIntervalIntMap::codomain_type({BI})));
  }
}

ChangeStatus Section::updateExtent() {
  std::optional<AddrRange> NewExtent;
  if (!ByteIntervals.empty()) {
    // Any ByteIntervals without an address will be at the front of the map
    // because nullopt sorts lower than any address.
    if (std::optional<Addr> Lower = (*ByteIntervals.begin())->getAddress()) {
      // All the ByteIntervals have an address, so we can calculate the
      // Section's extent. Get the address of the last ByteInterval in case it
      // has zero size; ByteIntervalAddrs does not track empty ByteIntervals.
      Addr Upper = *(*ByteIntervals.rbegin())->getAddress();
      if (!ByteIntervalAddrs.empty()) {
        // The last address is the max of the first address in the last
        // interval and the last address in intervals with non-zero size.
        Upper = std::max(Upper, ByteIntervalAddrs.rbegin()->first.upper());
      }
      NewExtent = AddrRange{*Lower, static_cast<uint64_t>(Upper - *Lower)};
    }
  }

  if (NewExtent != Extent) {
    if (Observer) {
      [[maybe_unused]] ChangeStatus Status = Observer->changeExtent(
          this, [&NewExtent](Section* S) { S->Extent = NewExtent; });
      assert(Status != ChangeStatus::Rejected &&
             "recovering from rejected extent changes is unimplemented");
    } else {
      Extent = NewExtent;
    }
    return ChangeStatus::Accepted;
  }
  return ChangeStatus::NoChange;
}

ChangeStatus Section::ByteIntervalObserverImpl::addCodeBlocks(
    [[maybe_unused]] ByteInterval* BI, ByteInterval::code_block_range Blocks) {
  if (S->Observer) {
    [[maybe_unused]] auto& Index = S->ByteIntervals.get<by_pointer>();
    assert(Index.find(BI) != Index.end() &&
           "byte interval observed by non-owner");
    // code_block_iterator takes a range of ranges, so wrap the given block
    // range in a one-element array.
    std::array<decltype(Blocks), 1> Range{Blocks};
    return S->Observer->addCodeBlocks(
        S, boost::make_iterator_range(code_block_iterator(Range),
                                      code_block_iterator()));
  }
  return ChangeStatus::NoChange;
}

ChangeStatus Section::ByteIntervalObserverImpl::moveCodeBlocks(
    [[maybe_unused]] ByteInterval* BI, ByteInterval::code_block_range Blocks) {
  if (S->Observer) {
    [[maybe_unused]] auto& Index = S->ByteIntervals.get<by_pointer>();
    assert(Index.find(BI) != Index.end() &&
           "byte interval observed by non-owner");
    // code_block_iterator takes a range of ranges, so wrap the given block
    // range in a one-element array.
    std::array<decltype(Blocks), 1> Range{Blocks};
    return S->Observer->moveCodeBlocks(
        S, boost::make_iterator_range(code_block_iterator(Range),
                                      code_block_iterator()));
  }
  return ChangeStatus::NoChange;
}

ChangeStatus Section::ByteIntervalObserverImpl::removeCodeBlocks(
    [[maybe_unused]] ByteInterval* BI, ByteInterval::code_block_range Blocks) {
  if (S->Observer) {
    [[maybe_unused]] auto& Index = S->ByteIntervals.get<by_pointer>();
    assert(Index.find(BI) != Index.end() &&
           "byte interval observed by non-owner");
    // code_block_iterator takes a range of ranges, so wrap the given block
    // range in a one-element array.
    std::array<decltype(Blocks), 1> Range{Blocks};
    return S->Observer->removeCodeBlocks(
        S, boost::make_iterator_range(code_block_iterator(Range),
                                      code_block_iterator()));
  }
  return ChangeStatus::NoChange;
}

ChangeStatus Section::ByteIntervalObserverImpl::addDataBlocks(
    [[maybe_unused]] ByteInterval* BI, ByteInterval::data_block_range Blocks) {
  if (S->Observer) {
    [[maybe_unused]] auto& Index = S->ByteIntervals.get<by_pointer>();
    assert(Index.find(BI) != Index.end() &&
           "byte interval observed by non-owner");
    // data_block_iterator takes a range of ranges, so wrap the given block
    // range in a one-element array.
    std::array<decltype(Blocks), 1> Range{Blocks};
    return S->Observer->addDataBlocks(
        S, boost::make_iterator_range(data_block_iterator(Range),
                                      data_block_iterator()));
  }
  return ChangeStatus::NoChange;
}

ChangeStatus Section::ByteIntervalObserverImpl::moveDataBlocks(
    [[maybe_unused]] ByteInterval* BI, ByteInterval::data_block_range Blocks) {
  if (S->Observer) {
    [[maybe_unused]] auto& Index = S->ByteIntervals.get<by_pointer>();
    assert(Index.find(BI) != Index.end() &&
           "byte interval observed by non-owner");
    // data_block_iterator takes a range of ranges, so wrap the given block
    // range in a one-element array.
    std::array<decltype(Blocks), 1> Range{Blocks};
    return S->Observer->moveDataBlocks(
        S, boost::make_iterator_range(data_block_iterator(Range),
                                      data_block_iterator()));
  }
  return ChangeStatus::NoChange;
}

ChangeStatus Section::ByteIntervalObserverImpl::removeDataBlocks(
    [[maybe_unused]] ByteInterval* BI, ByteInterval::data_block_range Blocks) {
  if (S->Observer) {
    [[maybe_unused]] auto& Index = S->ByteIntervals.get<by_pointer>();
    assert(Index.find(BI) != Index.end() &&
           "byte interval observed by non-owner");
    // data_block_iterator takes a range of ranges, so wrap the given block
    // range in a one-element array.
    std::array<decltype(Blocks), 1> Range{Blocks};
    return S->Observer->removeDataBlocks(
        S, boost::make_iterator_range(data_block_iterator(Range),
                                      data_block_iterator()));
  }
  return ChangeStatus::NoChange;
}

ChangeStatus Section::ByteIntervalObserverImpl::changeExtent(
    ByteInterval* BI, std::function<void(ByteInterval*)> Callback) {
  auto& Index = S->ByteIntervals.get<by_pointer>();
  if (auto It = Index.find(BI); It != Index.end()) {
    S->removeByteIntervalAddrs(BI);
    Index.modify(It, Callback);
    S->insertByteIntervalAddrs(BI);
  }

  return S->updateExtent();
}
