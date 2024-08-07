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
#include "CFGSerialization.hpp"
#include "Serialization.hpp"
#include <gtirb/DataBlock.hpp>
#include <gtirb/IR.hpp>
#include <gtirb/Module.hpp>
#include <gtirb/Section.hpp>
#include <gtirb/Symbol.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <gtirb/proto/IR.pb.h>
#include <google/protobuf/io/coded_stream.h>
#include <google/protobuf/io/zero_copy_stream_impl.h>
#include <google/protobuf/util/json_util.h>
#include <iostream>
#include <memory>

using namespace gtirb;

static constexpr const char* GTIRB_MAGIC_CHARS = "GTIRB";

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
    return ChangeStatus::Accepted;
  }

  ChangeStatus addProxyBlocks(Module* /*M*/,
                              Module::proxy_block_range Blocks) override {
    ChangeStatus Status = ChangeStatus::NoChange;
    if (!Blocks.empty()) {
      for (ProxyBlock& PB : Blocks) {
        // User could have called addVertex themselves, so check whether we
        // actually modified the graph.
        if (addVertex(&PB, I->Cfg).second)
          Status = ChangeStatus::Accepted;
      }
    }
    return Status;
  }

  ChangeStatus removeProxyBlocks(Module* /*M*/,
                                 Module::proxy_block_range Blocks) override {
    ChangeStatus Status = ChangeStatus::NoChange;
    if (!Blocks.empty()) {
      for (ProxyBlock& PB : Blocks) {
        // User could have called removeVertex themselves, so check whether
        // we actually modified the graph.
        if (removeVertex(&PB, I->Cfg))
          Status = ChangeStatus::Accepted;
      }
    }
    return Status;
  }

  ChangeStatus addCodeBlocks(Module* /*M*/,
                             Module::code_block_range Blocks) override {
    ChangeStatus Status = ChangeStatus::NoChange;
    if (!Blocks.empty()) {
      for (CodeBlock& CB : Blocks) {
        // User could have called addVertex themselves, so check whether we
        // actually modified the graph.
        if (addVertex(&CB, I->Cfg).second)
          Status = ChangeStatus::Accepted;
      }
    }
    return Status;
  }

  ChangeStatus removeCodeBlocks(Module* /*M*/,
                                Module::code_block_range Blocks) override {
    ChangeStatus Status = ChangeStatus::NoChange;
    if (!Blocks.empty()) {
      for (CodeBlock& CB : Blocks) {
        // User could have called removeVertex themselves, so check whether
        // we actually modified the graph.
        if (removeVertex(&CB, I->Cfg))
          Status = ChangeStatus::Accepted;
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

IR::IR(Context& C, const UUID& U)
    : AuxDataContainer(C, Kind::IR, U),
      MO(std::make_unique<ModuleObserverImpl>(this)) {}

class IRLoadErrorCategory : public std::error_category {
public:
  [[nodiscard]] const char* name() const noexcept override {
    return "gt.gtirb.ir";
  }
  [[nodiscard]] std::string message(int Condition) const override {
    switch (static_cast<IR::load_error>(Condition)) {
    case IR::load_error::IncorrectVersion:
      return "Incompatible protobuf version";
    case IR::load_error::CorruptFile:
      return "Corrupted GTIRB file";
    case IR::load_error::CorruptModule:
      return "Corrupted GTIRB module";
    case IR::load_error::CorruptSection:
      return "Corrupted GTIRB section";
    case IR::load_error::CorruptByteInterval:
      return "Corrupted byte interval";
    case IR::load_error::CorruptCFG:
      return "Error in parsing CFG";
    case IR::load_error::BadUUID:
      return "Bytes not valid UUID";
    case IR::load_error::MissingUUID:
      return "Could not locate UUID";
    case IR::load_error::NotGTIRB:
      return "File does not contain GTIRB";
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

ErrorOr<IR*> IR::fromProtobuf(Context& C, const MessageType& Message) {
  UUID Id;
  if (!uuidFromBytes(Message.uuid(), Id))
    return {load_error::CorruptFile, "Cannot load IR"};

  auto* I = IR::Create(C, Id);
  int i = 0;
  for (const auto& Elt : Message.modules()) {
    auto M = Module::fromProtobuf(C, Elt);
    if (!M) {
      ErrorInfo Err{load_error::CorruptModule, "#" + std::to_string(i)};
      Err.Msg += "\n" + M.getError().message();
      return Err;
    }
    I->addModule(*M);
    ++i;
  }
  if (!gtirb::fromProtobuf(C, I->Cfg, Message.cfg()))
    return load_error::CorruptCFG;
  static_cast<AuxDataContainer*>(I)->fromProtobuf(Message);
  I->Version = Message.version();

  if (I->Version != GTIRB_PROTOBUF_VERSION) {
    std::stringstream ss;
    ss << I->Version << "; expected version " << GTIRB_PROTOBUF_VERSION;
    return {load_error::IncorrectVersion, ss.str()};
  }
  return I;
}

void IR::save(std::ostream& Out) const {
  // Magic signature
  // Magic signature
  // Bytes 0-4 contain the ASCII characters: GTIRB.
  // Bytes 5-6 are considered reserved for future use and should be 0.
  // Byte 7 contains the GTIRB protobuf spec version in use.
  Out << GTIRB_MAGIC_CHARS << static_cast<uint8_t>(0) << static_cast<uint8_t>(0)
      << static_cast<uint8_t>(GTIRB_PROTOBUF_VERSION);

  // Protobuf
  MessageType Message;
  this->toProtobuf(&Message);
  Message.SerializeToOstream(&Out);
}

ErrorOr<IR*> IR::load(Context& C, std::istream& In) {
  constexpr size_t magic_len =
      std::string::traits_type::length(GTIRB_MAGIC_CHARS);
  std::array<char, magic_len> magic;
  In.read(magic.data(), magic_len);
  if (memcmp(magic.data(), GTIRB_MAGIC_CHARS, magic_len) != 0) {
    return {load_error::NotGTIRB, "GTIRB magic signature not found"};
  }

  uint8_t res0;
  In >> res0;

  uint8_t res1;
  In >> res1;

  uint8_t protobuf_version;
  In >> protobuf_version;
  if (protobuf_version != GTIRB_PROTOBUF_VERSION) {
    std::stringstream ss;
    ss << "GTIRB protobuf version mismatch. Expected: "
       << GTIRB_PROTOBUF_VERSION << " Saw: " << protobuf_version;
    return {load_error::IncorrectVersion, ss.str()};
  }

  google::protobuf::io::IstreamInputStream InputStream(&In);
  google::protobuf::io::CodedInputStream CodedStream(&InputStream);
#ifdef PROTOBUF_SET_BYTES_LIMIT
  CodedStream.SetTotalBytesLimit(INT_MAX, INT_MAX);
#endif

  MessageType Message;
  if (!Message.ParseFromCodedStream(&CodedStream)) {
    return {load_error::CorruptFile, "Protobuf unable to be parsed"};
  }

  return IR::fromProtobuf(C, Message);
}

void IR::saveJSON(std::ostream& Out) const {
  MessageType Message;
  this->toProtobuf(&Message);
  std::string S;
  auto status = google::protobuf::util::MessageToJsonString(Message, &S);
  if (!status.ok()) {
    Out << status;
    return;
  } else {
    Out << S;
  }
}

ErrorOr<IR*> IR::loadJSON(Context& C, std::istream& In) {
  MessageType Message;
  std::string S;
  auto status = google::protobuf::util::JsonStringToMessage(
      std::string(std::istreambuf_iterator<char>(In), {}), &Message);
  if (!status.ok()) {
    return {load_error::CorruptFile, status.ToString()};
  } else {
    return IR::fromProtobuf(C, Message);
  }
}
