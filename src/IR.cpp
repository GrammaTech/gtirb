//===- IR.cpp ---------------------------------------------------*- C++ -*-===//
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
#include "IR.hpp"
#include "CFGSerialization.hpp"
#include "Serialization.hpp"
#include <gtirb/DataBlock.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <gtirb/proto/IR.pb.h>
#include <google/protobuf/util/json_util.h>

using namespace gtirb;

class IR::ModuleObserverImpl : public ModuleObserver {
public:
  explicit ModuleObserverImpl(IR* I_) : I(I_) {}

  ChangeStatus nameChange(Module* M, const std::string& /*OldName*/,
                          const std::string& /*NewName*/) override {
    auto& Index = I->Modules.get<by_pointer>();
    auto It = Index.find(M);
    assert(It != Index.end() && "module observed by non-owner");
    // The lambda would ordinarily update the Module such that the result
    // of Module::getName changes. Because that change happened before this
    // method was called, the lambda doesn't need to do anything.
    Index.modify(It, [](Module*) {});
    return ChangeStatus::ACCEPTED;
  }

  ChangeStatus addProxyBlocks(Module* /*M*/,
                              Module::proxy_block_range Blocks) override {
    ChangeStatus Status = ChangeStatus::NO_CHANGE;
    if (!Blocks.empty()) {
      for (ProxyBlock& PB : Blocks) {
        // User could have called addVertex themselves, so check whether we
        // actually modified the graph.
        if (addVertex(&PB, I->Cfg).second)
          Status = ChangeStatus::ACCEPTED;
      }
    }
    return Status;
  }

  ChangeStatus removeProxyBlocks(Module* /*M*/,
                                 Module::proxy_block_range Blocks) override {
    ChangeStatus Status = ChangeStatus::NO_CHANGE;
    if (!Blocks.empty()) {
      for (ProxyBlock& PB : Blocks) {
        // User could have called removeVertex themselves, so check whether
        // we actually modified the graph.
        if (removeVertex(&PB, I->Cfg))
          Status = ChangeStatus::ACCEPTED;
      }
    }
    return Status;
  }

  ChangeStatus addCodeBlocks(Module* /*M*/,
                             Module::code_block_range Blocks) override {
    ChangeStatus Status = ChangeStatus::NO_CHANGE;
    if (!Blocks.empty()) {
      for (CodeBlock& CB : Blocks) {
        // User could have called addVertex themselves, so check whether we
        // actually modified the graph.
        if (addVertex(&CB, I->Cfg).second)
          Status = ChangeStatus::ACCEPTED;
      }
    }
    return Status;
  }

  ChangeStatus removeCodeBlocks(Module* /*M*/,
                                Module::code_block_range Blocks) override {
    ChangeStatus Status = ChangeStatus::NO_CHANGE;
    if (!Blocks.empty()) {
      for (CodeBlock& CB : Blocks) {
        // User could have called removeVertex themselves, so check whether
        // we actually modified the graph.
        if (removeVertex(&CB, I->Cfg))
          Status = ChangeStatus::ACCEPTED;
      }
    }
    return Status;
  }

private:
  IR* I;
};

IR::IR(Context& C)
    : AuxDataContainer(C, Kind::IR),
      MO(std::make_unique<ModuleObserverImpl>(this)) {}

class IRLoadErrorCategory : public std::error_category {
public:
  const char* name() const noexcept override { return "gt.gtirb.ir"; }
  std::string message(int Condition) const override {
    switch (static_cast<IR::load_error>(Condition)) {
    case IR::load_error::IncorrectVersion:
      return "GTIRB file has an incompatible version number";
    case IR::load_error::CorruptFile:
      return "Corrupted GTIRB file";
    }
    assert(false && "Expected to handle all error codes");
    return "";
  }
};

const std::error_category& gtirb::loadErrorCategory() {
  static IRLoadErrorCategory Cat;
  return Cat;
}

void IR::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  *Message->mutable_cfg() = gtirb::toProtobuf(this->Cfg);
  containerToProtobuf(this->Modules, Message->mutable_modules());
  AuxDataContainer::toProtobuf(Message);
  Message->set_version(Version);
}

IR* IR::fromProtobuf(Context& C, const MessageType& Message) {
  auto* I = IR::Create(C);
  if (!setNodeUUIDFromBytes(I, Message.uuid()))
    return nullptr;
  for (const auto& Elt : Message.modules()) {
    auto* M = Module::fromProtobuf(C, Elt);
    if (!M)
      return nullptr;
    I->addModule(M);
  }
  if (!gtirb::fromProtobuf(C, I->Cfg, Message.cfg()))
    return nullptr;
  static_cast<AuxDataContainer*>(I)->fromProtobuf(Message);
  I->Version = Message.version();
  return I;
}

void IR::save(std::ostream& Out) const {
  MessageType Message;
  this->toProtobuf(&Message);
  Message.SerializeToOstream(&Out);
}

ErrorOr<IR*> IR::load(Context& C, std::istream& In) {
  MessageType Message;
  Message.ParseFromIstream(&In);

  auto* I = IR::fromProtobuf(C, Message);
  if (!I) {
    return IR::load_error::CorruptFile;
  }
  if (I->Version != GTIRB_PROTOBUF_VERSION) {
    return IR::load_error::IncorrectVersion;
  }
  return I;
}

void IR::saveJSON(std::ostream& Out) const {
  MessageType Message;
  this->toProtobuf(&Message);
  std::string S;
  google::protobuf::util::MessageToJsonString(Message, &S);
  Out << S;
}

ErrorOr<IR*> IR::loadJSON(Context& C, std::istream& In) {
  MessageType Message;
  std::string S;
  google::protobuf::util::JsonStringToMessage(
      std::string(std::istreambuf_iterator<char>(In), {}), &Message);

  auto* I = IR::fromProtobuf(C, Message);
  if (!I) {
    return IR::load_error::CorruptFile;
  }
  if (I->Version != GTIRB_PROTOBUF_VERSION) {
    return IR::load_error::IncorrectVersion;
  }
  return I;
}
