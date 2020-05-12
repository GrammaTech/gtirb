//===- Module.cpp -----------------------------------------------*- C++ -*-===//
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
#include "Module.hpp"
#include "Serialization.hpp"
#include <gtirb/CFG.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <map>

using namespace gtirb;

void Module::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_binary_path(this->BinaryPath);
  Message->set_preferred_addr(static_cast<uint64_t>(this->PreferredAddr));
  Message->set_rebase_delta(this->RebaseDelta);
  Message->set_file_format(static_cast<proto::FileFormat>(this->FileFormat));
  Message->set_isa(static_cast<proto::ISA>(this->Isa));
  Message->set_name(this->Name);
  sequenceToProtobuf(ProxyBlocks.begin(), ProxyBlocks.end(),
                     Message->mutable_proxies());
  sequenceToProtobuf(sections_begin(), sections_end(),
                     Message->mutable_sections());
  containerToProtobuf(Symbols, Message->mutable_symbols());
  if (EntryPoint) {
    nodeUUIDToBytes(EntryPoint, *Message->mutable_entry_point());
  }
  AuxDataContainer::toProtobuf(Message);
}

// FIXME: improve containerFromProtobuf so it can handle a pair where one
// element is a pointer to a Node subclass.
template <class T, class U, class V, class W>
static void nodeMapFromProtobuf(Context& C, std::map<T, U*>& Values,
                                const google::protobuf::Map<V, W>& Message) {
  Values.clear();
  std::for_each(Message.begin(), Message.end(), [&Values, &C](const auto& M) {
    std::pair<T, U*> Val;
    fromProtobuf(C, Val.first, M.first);
    Val.second = U::fromProtobuf(C, M.second);
    Values.insert(std::move(Val));
  });
}

Module* Module::fromProtobuf(Context& C, const MessageType& Message) {
  Module* M = Module::Create(C);
  if (!setNodeUUIDFromBytes(M, Message.uuid()))
    return nullptr;
  M->BinaryPath = Message.binary_path();
  M->PreferredAddr = Addr(Message.preferred_addr());
  M->RebaseDelta = Message.rebase_delta();
  M->FileFormat = static_cast<gtirb::FileFormat>(Message.file_format());
  M->Isa = static_cast<ISA>(Message.isa());
  M->Name = Message.name();
  for (const auto& Elt : Message.proxies()) {
    auto* PB = ProxyBlock::fromProtobuf(C, Elt);
    if (!PB)
      return nullptr;
    M->ProxyBlocks.insert(PB);
  }
  for (const auto& Elt : Message.sections()) {
    auto* S = Section::fromProtobuf(C, Elt);
    if (!S)
      return nullptr;
    M->addSection(S);
  }
  for (const auto& Elt : Message.symbols()) {
    auto* S = Symbol::fromProtobuf(C, Elt);
    if (!S)
      return nullptr;
    M->addSymbol(S);
  }
  for (const auto& ProtoS : Message.sections()) {
    for (const auto& ProtoBI : ProtoS.byte_intervals()) {
      UUID Id;
      if (!uuidFromBytes(ProtoBI.uuid(), Id))
        return nullptr;
      auto* BI = dyn_cast_or_null<ByteInterval>(getByUUID(C, Id));
      if (!BI)
        return nullptr;
      if (!BI->symbolicExpressionsFromProtobuf(C, ProtoBI))
        return nullptr;
    }
  }
  if (!Message.entry_point().empty()) {
    UUID Id;
    if (!uuidFromBytes(Message.entry_point(), Id))
      return nullptr;
    M->EntryPoint = dyn_cast_or_null<CodeBlock>(Node::getByUUID(C, Id));
    if (!M->EntryPoint)
      return nullptr;
  }
  static_cast<AuxDataContainer*>(M)->fromProtobuf(Message);
  return M;
}

ChangeStatus Module::removeProxyBlock(ProxyBlock* B) {
  if (auto It = ProxyBlocks.find(B); It != ProxyBlocks.end()) {
    if (Observer) {
      auto BlockRange = boost::make_iterator_range(It, std::next(It));
      [[maybe_unused]] ChangeStatus status =
          Observer->removeProxyBlocks(this, BlockRange);
      // The known observers do not reject removals. If that changes, this
      // method must be updated. Because addProxyBlock(ProxyBlock*) also
      // assumes removals are never rejected, that method should be updated
      // as well.
      assert(status != ChangeStatus::REJECTED &&
             "recovering from rejected removal is unimplemented");
    }
    ProxyBlocks.erase(It);
    B->setModule(nullptr);
    return ChangeStatus::ACCEPTED;
  }
  return ChangeStatus::NO_CHANGE;
}

ChangeStatus Module::addProxyBlock(ProxyBlock* B) {
  if (Module* M = B->getModule()) {
    if (M == this)
      return ChangeStatus::NO_CHANGE;
    [[maybe_unused]] ChangeStatus status = M->removeProxyBlock(B);
    assert(status != ChangeStatus::REJECTED &&
           "failed to remove block from former parent");
  }

  B->setModule(this);
  auto [It, Inserted] = ProxyBlocks.insert(B);
  if (Inserted && Observer) {
    auto BlockRange = boost::make_iterator_range(It, std::next(It));
    [[maybe_unused]] ChangeStatus status =
        Observer->addProxyBlocks(this, BlockRange);
    // The known observers do not reject insertions. If that changes, this
    // method must be updated.
    assert(status != ChangeStatus::REJECTED &&
           "recovering from rejected insertion is unimplemented");
  }
  return ChangeStatus::ACCEPTED;
}

// Present for testing purposes only.
void Module::save(std::ostream& Out) const {
  MessageType Message;
  this->toProtobuf(&Message);
  Message.SerializeToOstream(&Out);
}

// Present for testing purposes only.
Module* Module::load(Context& C, std::istream& In) {
  MessageType Message;
  Message.ParseFromIstream(&In);
  auto M = Module::fromProtobuf(C, Message);
  return M;
}

ChangeStatus Module::removeSection(Section* S) {
  auto& Index = Sections.get<by_pointer>();
  if (auto Iter = Index.find(S); Iter != Index.end()) {
    [[maybe_unused]] ChangeStatus Status =
        SecObs.changeExtent(S, addressRange(*S), std::nullopt);
    assert(Status != ChangeStatus::REJECTED &&
           "failed to update Module extent when removing section");

    if (Observer) {
      auto Begin = Sections.project<by_address>(Iter);
      auto End = std::next(Begin);
      auto BlockRange = makeCodeBlockRange(Begin, End);
      Status = Observer->removeCodeBlocks(this, BlockRange);
      // The known observers do not reject removals. If that changes, this
      // method must be updated. Because addSection(Section*) also assumes
      // removals are never rejected, that method should be updated as well.
      assert(Status != ChangeStatus::REJECTED &&
             "recovering from rejected removal is unimplemented");
    }
    Index.erase(Iter);

    S->setParent(nullptr, nullptr);
    return ChangeStatus::ACCEPTED;
  }
  return ChangeStatus::NO_CHANGE;
}

ChangeStatus Module::addSection(Section* S) {
  if (Module* M = S->getModule()) {
    if (M == this)
      return ChangeStatus::NO_CHANGE;
    [[maybe_unused]] ChangeStatus Status = M->removeSection(S);
    assert(Status != ChangeStatus::REJECTED &&
           "failed to remove section from former parent");
  }

  S->setParent(this, &SecObs);

  auto [Iter, Inserted] = Sections.emplace(S);
  if (Inserted && Observer) {
    auto BlockRange = makeCodeBlockRange(Iter, std::next(Iter));
    [[maybe_unused]] ChangeStatus Status =
        Observer->addCodeBlocks(this, BlockRange);
    // The known observers do not reject insertions. If that changes, this
    // method must be updated.
    assert(Status != ChangeStatus::REJECTED &&
           "recovering from rejected insertion is unimplemented");
  }

  [[maybe_unused]] ChangeStatus Status =
      SecObs.changeExtent(S, std::nullopt, addressRange(*S));
  assert(Status != ChangeStatus::REJECTED &&
         "failed to update Module extent after adding section");
  return ChangeStatus::ACCEPTED;
}

static auto NoOp = [](auto*) {};

ChangeStatus
Module::SectionObserverImpl::nameChange(Section* S,
                                        const std::string& /*OldName*/,
                                        const std::string& /*NewName*/) {
  auto& Index = M->Sections.get<by_pointer>();
  auto It = Index.find(S);
  assert(It != Index.end() && "section observed by non-owner");
  // The following lambda is intentionally a no-op. Because the Section's name
  // has already been updated before this method executes, we only need to tell
  // the index to re-synchronize.
  Index.modify(It, NoOp);
  return ChangeStatus::ACCEPTED;
}

ChangeStatus
Module::SectionObserverImpl::addCodeBlocks([[maybe_unused]] Section* S,
                                           Section::code_block_range Blocks) {
  ChangeStatus Status = ChangeStatus::NO_CHANGE;
  if (M->Observer) {
    [[maybe_unused]] auto& SectionIndex = M->Sections.get<by_pointer>();
    assert(SectionIndex.find(S) != SectionIndex.end() &&
           "section observed by non-owner");
    // code_block_iterator takes a range of ranges, so wrap the given block
    // range in a one-element array.
    std::array Range{Blocks};
    Status = M->Observer->addCodeBlocks(
        M, boost::make_iterator_range(code_block_iterator(Range),
                                      code_block_iterator()));
    assert(Status != ChangeStatus::REJECTED &&
           "recovering from rejected insertion is not implemented");
  }

  if (moveCodeBlocks(S, Blocks) == ChangeStatus::ACCEPTED)
    return ChangeStatus::ACCEPTED;
  return Status;
}

ChangeStatus
Module::SectionObserverImpl::moveCodeBlocks([[maybe_unused]] Section* S,
                                            Section::code_block_range Blocks) {
  ChangeStatus Status = ChangeStatus::NO_CHANGE;
  auto& Index = M->Symbols.get<by_referent>();
  for (CodeBlock& Block : Blocks) {
    for (auto [It, End] = Index.equal_range(&Block); It != End; ++It) {
      // Re-synchronize the address associated with the symbol. This should not
      // invalidate the iterators into the referent index because only the
      // address index needs to be updated.
      Index.modify(It, NoOp);
      Status = ChangeStatus::ACCEPTED;
    }
  }

  return Status;
}

ChangeStatus Module::SectionObserverImpl::removeCodeBlocks(
    [[maybe_unused]] Section* S, Section::code_block_range Blocks) {
  ChangeStatus Status = ChangeStatus::NO_CHANGE;
  if (M->Observer) {
    [[maybe_unused]] auto& SectionIndex = M->Sections.get<by_pointer>();
    assert(SectionIndex.find(S) != SectionIndex.end() &&
           "section observed by non-owner");
    // code_block_iterator takes a range of ranges, so wrap the given block
    // range in a one-element array.
    std::array Range{Blocks};
    Status = M->Observer->removeCodeBlocks(
        M, boost::make_iterator_range(code_block_iterator(Range),
                                      code_block_iterator()));
    assert(Status != ChangeStatus::REJECTED &&
           "recovering from failed removal is not implemented");
  }

  if (moveCodeBlocks(S, Blocks) == ChangeStatus::ACCEPTED)
    return ChangeStatus::ACCEPTED;
  return Status;
}

ChangeStatus
Module::SectionObserverImpl::addDataBlocks(Section* S,
                                           Section::data_block_range Blocks) {
  return moveDataBlocks(S, Blocks);
}

ChangeStatus
Module::SectionObserverImpl::moveDataBlocks(Section* /* S */,
                                            Section::data_block_range Blocks) {
  ChangeStatus Status = ChangeStatus::NO_CHANGE;
  auto& Index = M->Symbols.get<by_referent>();
  for (DataBlock& Block : Blocks) {
    for (auto [It, End] = Index.equal_range(&Block); It != End; ++It) {
      // Re-synchronize the address associated with the symbol. This should not
      // invalidate the iterators into the referent index because only the
      // address index needs to be updated.
      Index.modify(It, NoOp);
      Status = ChangeStatus::ACCEPTED;
    }
  }
  return Status;
}

ChangeStatus Module::SectionObserverImpl::removeDataBlocks(
    Section* S, Section::data_block_range Blocks) {
  return moveDataBlocks(S, Blocks);
}

ChangeStatus
Module::SectionObserverImpl::changeExtent(Section* S,
                                          std::optional<AddrRange> OldExtent,
                                          std::optional<AddrRange> NewExtent) {
  if (OldExtent == NewExtent)
    return ChangeStatus::NO_CHANGE;

  auto& Index = M->Sections.get<by_pointer>();
  if (auto It = Index.find(S); It != Index.end()) {
    std::optional<AddrRange> Previous = addressRange(*M);
    if (OldExtent)
      M->SectionAddrs.subtract(
          std::make_pair(SectionIntMap::interval_type::right_open(
                             OldExtent->lower(), OldExtent->upper()),
                         SectionIntMap::codomain_type({S})));

    // The following lambda is intentionally a no-op. Because the Section's
    // address has already been updated before this method executes, we only
    // need to tell the index to re-synchronize.
    Index.modify(It, NoOp);

    if (NewExtent)
      M->SectionAddrs.add(
          std::make_pair(SectionIntMap::interval_type::right_open(
                             NewExtent->lower(), NewExtent->upper()),
                         SectionIntMap::codomain_type({S})));

    if (Previous != addressRange(*M))
      return ChangeStatus::ACCEPTED;
  }
  return ChangeStatus::NO_CHANGE;
}

ChangeStatus Module::SymbolObserverImpl::nameChange(Symbol* S,
                                                    const std::string&,
                                                    const std::string&) {
  auto& Index = M->Symbols.get<by_pointer>();
  auto It = Index.find(S);
  assert(It != Index.end() && "symbol observed by non-owner");
  // The following lambda is intentionally a no-op. Because the Symbol's name
  // has already been updated before this method executes, we only need to tell
  // the index to re-synchronize.
  Index.modify(It, NoOp);
  return ChangeStatus::ACCEPTED;
}

ChangeStatus Module::SymbolObserverImpl::referentChange(
    Symbol* S, std::variant<std::monostate, Addr, Node*>,
    std::variant<std::monostate, Addr, Node*>) {
  auto& Index = M->Symbols.get<by_pointer>();
  auto It = Index.find(S);
  assert(It != Index.end() && "symbol observed by non-owner");
  // The following lambda is intentionally a no-op. Because the Symbol's
  // referent or address has already been updated before this method executes,
  // we only need to tell the index to re-synchronize.
  Index.modify(It, NoOp);
  return ChangeStatus::ACCEPTED;
}
