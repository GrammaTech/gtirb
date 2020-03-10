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

bool Section::removeByteInterval(ByteInterval* BI) {
  auto& Index = ByteIntervals.get<by_pointer>();
  if (auto Iter = Index.find(BI); Iter != Index.end()) {
    if (Parent) {
      auto Begin = ByteIntervals.project<by_address>(Iter);
      auto End = std::next(Begin);
      Parent->removeCodeBlocks(this, makeCodeBlockRange(Begin, End));
    }

    // Section::mutateIndices updates the Module's SectionAddrs and Sections
    // data structures in case removing the ByteInterval causes the range of
    // addresses in this Section to change...
    //
    // The ByteInterval is removed inside the lambda so that getAddress and
    // getSize return the old address and size, respectively, before the lambda
    // is invoked, and the new address and size afterwards.
    this->mutateIndices([&Index, Iter]() mutable { Index.erase(Iter); });

    // ByteInterval::removeFromIndices removes the ByteInterval from this
    // Section's ByteIntervalAddrs...

    BI->removeFromIndices();

    // Unset the ByteInterval's Section *after* calling removeFromIndices.

    BI->setSection(nullptr);
    return true;
  }
  return false;
}

ByteInterval* Section::addByteInterval(ByteInterval* BI) {
  if (BI->getSection()) {
    BI->getSection()->removeByteInterval(BI);
  }
  // Set the ByteInterval's Section *before* calling addToIndices.

  BI->setSection(this);

  // ByteInterval::addToIndices adds the ByteInterval to this Section's
  // ByteIntervalAddrs...

  BI->addToIndices();

  // Section::mutateIndices updates the Module's SectionAddrs and Sections data
  // structures in case the new ByteInterval caused the range of addresses in
  // this Section to change...
  //
  // The ByteInterval is emplaced inside the lambda so that getAddress and
  // getSize return the old address and size, respectively, before the lambda
  // is invoked, and the new address and size afterwards. It is not necessary
  // to call addCodeBlocks from inside the lambda, but it is convenient.

  this->mutateIndices([this, BI]() mutable {
    auto [Iter, Inserted] = ByteIntervals.emplace(BI);
    if (Inserted && Parent) {
      Parent->addCodeBlocks(this, makeCodeBlockRange(Iter, std::next(Iter)));
    }
  });
  return BI;
}
