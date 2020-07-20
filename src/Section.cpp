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

  ChangeStatus changeExtent(ByteInterval* BI,
                            std::optional<AddrRange> OldExtent,
                            std::optional<AddrRange> NewExtent) override;

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

Section* Section::fromProtobuf(Context& C, const MessageType& Message) {
  UUID Id;
  if (!uuidFromBytes(Message.uuid(), Id))
    return nullptr;

  auto* S = Section::Create(C, Message.name(), Id);
  for (int I = 0, E = Message.section_flags_size(); I != E; ++I) {
    S->addFlag(static_cast<SectionFlag>(Message.section_flags(I)));
  }
  for (const auto& ProtoInterval : Message.byte_intervals()) {
    auto* BI = ByteInterval::fromProtobuf(C, ProtoInterval);
    if (!BI)
      return nullptr;
    S->addByteInterval(BI);
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
  return S;
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
      // implementation will need to be changed as well. Because addByteInterval
      // assumes that removal will not be rejected, it will need to be updated.
      assert(Status != ChangeStatus::Rejected &&
             "recovering from rejected removal is not implemented yet");
    }
    Index.erase(Iter);
    BI->setParent(nullptr, nullptr);

    [[maybe_unused]] ChangeStatus Status =
        BIO->changeExtent(BI, addressRange(*BI), std::nullopt);
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

  [[maybe_unused]] ChangeStatus Status =
      BIO->changeExtent(BI, std::nullopt, addressRange(*BI));
  assert(Status != ChangeStatus::Rejected &&
         "failed to change Section extent after adding ByteInterval");
  return ChangeStatus::Accepted;
}

ChangeStatus Section::ByteIntervalObserverImpl::addCodeBlocks(
    [[maybe_unused]] ByteInterval* BI, ByteInterval::code_block_range Blocks) {
  if (S->Observer) {
    [[maybe_unused]] auto& Index = S->ByteIntervals.get<by_pointer>();
    assert(Index.find(BI) != Index.end() &&
           "byte interval observed by non-owner");
    // code_block_iterator takes a range of ranges, so wrap the given block
    // range in a one-element array.
    std::array Range{Blocks};
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
    std::array Range{Blocks};
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
    std::array Range{Blocks};
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
    std::array Range{Blocks};
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
    std::array Range{Blocks};
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
    std::array Range{Blocks};
    return S->Observer->removeDataBlocks(
        S, boost::make_iterator_range(data_block_iterator(Range),
                                      data_block_iterator()));
  }
  return ChangeStatus::NoChange;
}

ChangeStatus Section::ByteIntervalObserverImpl::changeExtent(
    ByteInterval* BI, std::optional<AddrRange> OldExtent,
    std::optional<AddrRange> NewExtent) {
  auto& Index = S->ByteIntervals.get<by_pointer>();
  if (auto It = Index.find(BI); It != Index.end()) {
    // The following lambda is intentionally a no-op. Because the ByteInterval's
    // address has already been updated before this method executes, we only
    // need to tell the index to re-synchronize.
    Index.modify(It, [](ByteInterval*) {});

    if (OldExtent) {
      S->ByteIntervalAddrs.subtract(
          std::make_pair(ByteIntervalIntMap::interval_type::right_open(
                             OldExtent->lower(), OldExtent->upper()),
                         ByteIntervalIntMap::codomain_type({BI})));
    }
    if (NewExtent) {
      S->ByteIntervalAddrs.add(
          std::make_pair(ByteIntervalIntMap::interval_type::right_open(
                             NewExtent->lower(), NewExtent->upper()),
                         ByteIntervalIntMap::codomain_type({BI})));
    }

    std::optional<AddrRange> Previous = S->Extent;
    S->Extent.reset();
    if (!S->ByteIntervals.empty()) {
      // Any ByteIntervals without an address will be at the front of the map
      // because nullopt sorts lower than any address.
      if (std::optional<Addr> Lower =
              (*S->ByteIntervals.begin())->getAddress()) {
        // All the ByteIntervals have an address, so we can calculate the
        // Section's extent. Get the address of the last ByteInterval in case it
        // has zero size; ByteIntervalAddrs does not track empty ByteIntervals.
        Addr Upper = *(*S->ByteIntervals.rbegin())->getAddress();
        if (!S->ByteIntervalAddrs.empty()) {
          // The last address is the max of the first address in the last
          // interval and the last address in intervals with non-zero size.
          Upper = std::max(Upper, S->ByteIntervalAddrs.rbegin()->first.upper());
        }
        S->Extent = AddrRange{*Lower, static_cast<uint64_t>(Upper - *Lower)};
      }
    }

    if (Previous != S->Extent) {
      if (S->Observer) {
        [[maybe_unused]] ChangeStatus Status =
            S->Observer->changeExtent(S, Previous, S->Extent);
        assert(Status != ChangeStatus::Rejected &&
               "recovering from rejected extent changes is unimplemented");
      }
      return ChangeStatus::Accepted;
    }
  }
  return ChangeStatus::NoChange;
}
