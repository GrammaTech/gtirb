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
  auto* S = Section::Create(C, Message.name());
  for (int I = 0, E = Message.section_flags_size(); I != E; ++I) {
    S->addFlag(static_cast<SectionFlag>(Message.section_flags(I)));
  }
  if (!setNodeUUIDFromBytes(S, Message.uuid()))
    return nullptr;
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
      assert(Status != ChangeStatus::REJECTED &&
             "recovering from rejected removal is not implemented yet");
    }
    Index.erase(Iter);
    BI->setParent(nullptr, nullptr);

    [[maybe_unused]] ChangeStatus Status =
        changeExtent(BI, addressRange(*BI), std::nullopt);
    assert(Status != ChangeStatus::REJECTED &&
           "failed to change Section extent after removing ByteInterval");
    return ChangeStatus::ACCEPTED;
  }
  return ChangeStatus::NO_CHANGE;
}

ChangeStatus Section::addByteInterval(ByteInterval* BI) {
  if (Section* S = BI->getSection()) {
    if (S == this) {
      return ChangeStatus::NO_CHANGE;
    }
    [[maybe_unused]] ChangeStatus Status = S->removeByteInterval(BI);
    assert(Status != ChangeStatus::REJECTED &&
           "failed to remove node from parent");
  }

  BI->setParent(this, this);
  auto [Iter, Inserted] = ByteIntervals.emplace(BI);
  if (Inserted && Observer) {
    auto Blocks = makeCodeBlockRange(Iter, std::next(Iter));
    [[maybe_unused]] ChangeStatus Status =
        Observer->addCodeBlocks(this, Blocks);
    // None of the known observers reject insertions. If that changes, this
    // implementation must be updated.
    assert(Status != ChangeStatus::REJECTED &&
           "recovering from rejected insertion is unimplemented");
  }

  [[maybe_unused]] ChangeStatus Status =
      changeExtent(BI, std::nullopt, addressRange(*BI));
  assert(Status != ChangeStatus::REJECTED &&
         "failed to change Section extent after adding ByteInterval");
  return ChangeStatus::ACCEPTED;
}

ChangeStatus Section::addCodeBlocks(ByteInterval* BI,
                                    ByteInterval::code_block_range Blocks) {
  if (Observer) {
    [[maybe_unused]] auto& Index = ByteIntervals.get<by_pointer>();
    assert(Index.find(BI) != Index.end() &&
           "byte interval observed by non-owner");
    // code_block_iterator takes a range of ranges, so wrap the given block
    // range in a one-element array.
    std::array Range{Blocks};
    return Observer->addCodeBlocks(
        this, boost::make_iterator_range(code_block_iterator(Range),
                                         code_block_iterator()));
  }
  return ChangeStatus::NO_CHANGE;
}

ChangeStatus Section::moveCodeBlocks(ByteInterval* BI,
                                     ByteInterval::code_block_range Blocks) {
  if (Observer) {
    [[maybe_unused]] auto& Index = ByteIntervals.get<by_pointer>();
    assert(Index.find(BI) != Index.end() &&
           "byte interval observed by non-owner");
    // code_block_iterator takes a range of ranges, so wrap the given block
    // range in a one-element array.
    std::array Range{Blocks};
    return Observer->moveCodeBlocks(
        this, boost::make_iterator_range(code_block_iterator(Range),
                                         code_block_iterator()));
  }
  return ChangeStatus::NO_CHANGE;
}

ChangeStatus Section::removeCodeBlocks(ByteInterval* BI,
                                       ByteInterval::code_block_range Blocks) {
  if (Observer) {
    [[maybe_unused]] auto& Index = ByteIntervals.get<by_pointer>();
    assert(Index.find(BI) != Index.end() &&
           "byte interval observed by non-owner");
    // code_block_iterator takes a range of ranges, so wrap the given block
    // range in a one-element array.
    std::array Range{Blocks};
    return Observer->removeCodeBlocks(
        this, boost::make_iterator_range(code_block_iterator(Range),
                                         code_block_iterator()));
  }
  return ChangeStatus::NO_CHANGE;
}

ChangeStatus Section::addDataBlocks(ByteInterval* BI,
                                    ByteInterval::data_block_range Blocks) {
  if (Observer) {
    [[maybe_unused]] auto& Index = ByteIntervals.get<by_pointer>();
    assert(Index.find(BI) != Index.end() &&
           "byte interval observed by non-owner");
    // data_block_iterator takes a range of ranges, so wrap the given block
    // range in a one-element array.
    std::array Range{Blocks};
    return Observer->addDataBlocks(
        this, boost::make_iterator_range(data_block_iterator(Range),
                                         data_block_iterator()));
  }
  return ChangeStatus::NO_CHANGE;
}

ChangeStatus Section::moveDataBlocks(ByteInterval* BI,
                                     ByteInterval::data_block_range Blocks) {
  if (Observer) {
    [[maybe_unused]] auto& Index = ByteIntervals.get<by_pointer>();
    assert(Index.find(BI) != Index.end() &&
           "byte interval observed by non-owner");
    // data_block_iterator takes a range of ranges, so wrap the given block
    // range in a one-element array.
    std::array Range{Blocks};
    return Observer->moveDataBlocks(
        this, boost::make_iterator_range(data_block_iterator(Range),
                                         data_block_iterator()));
  }
  return ChangeStatus::NO_CHANGE;
}

ChangeStatus Section::removeDataBlocks(ByteInterval* BI,
                                       ByteInterval::data_block_range Blocks) {
  if (Observer) {
    [[maybe_unused]] auto& Index = ByteIntervals.get<by_pointer>();
    assert(Index.find(BI) != Index.end() &&
           "byte interval observed by non-owner");
    // data_block_iterator takes a range of ranges, so wrap the given block
    // range in a one-element array.
    std::array Range{Blocks};
    return Observer->removeDataBlocks(
        this, boost::make_iterator_range(data_block_iterator(Range),
                                         data_block_iterator()));
  }
  return ChangeStatus::NO_CHANGE;
}

ChangeStatus Section::changeExtent(ByteInterval* BI,
                                   std::optional<AddrRange> OldExtent,
                                   std::optional<AddrRange> NewExtent) {
  auto& Index = ByteIntervals.get<by_pointer>();
  if (auto It = Index.find(BI); It != Index.end()) {
    // The following lambda is intentionally a no-op. Because the ByteInterval's
    // address has already been updated before this method executes, we only
    // need to tell the index to re-synchronize.
    Index.modify(It, [](ByteInterval*) {});

    if (OldExtent) {
      ByteIntervalAddrs.subtract(
          std::make_pair(ByteIntervalIntMap::interval_type::right_open(
                             OldExtent->lower(), OldExtent->upper()),
                         ByteIntervalIntMap::codomain_type({BI})));
    }
    if (NewExtent) {
      ByteIntervalAddrs.add(
          std::make_pair(ByteIntervalIntMap::interval_type::right_open(
                             NewExtent->lower(), NewExtent->upper()),
                         ByteIntervalIntMap::codomain_type({BI})));
    }

    std::optional<AddrRange> Previous = Extent;
    Extent.reset();
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
        Extent = AddrRange{*Lower, static_cast<uint64_t>(Upper - *Lower)};
      }
    }

    if (Previous != Extent) {
      if (Observer) {
        [[maybe_unused]] ChangeStatus Status =
            Observer->changeExtent(this, Previous, Extent);
        assert(Status != ChangeStatus::REJECTED &&
               "recovering from rejected extent changes is unimplemented");
      }
      return ChangeStatus::ACCEPTED;
    }
  }
  return ChangeStatus::NO_CHANGE;
}
