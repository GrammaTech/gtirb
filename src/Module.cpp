//===- Module.cpp -----------------------------------------------*- C++ -*-===//
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
#include "Module.hpp"
#include "Serialization.hpp"
#include <gtirb/Block.hpp>
#include <gtirb/CFG.hpp>
#include <gtirb/ImageByteMap.hpp>
#include <gtirb/SymbolicExpression.hpp>
#include <proto/Module.pb.h>
#include <map>

using namespace gtirb;

Module::Module(Context& C)
    : AuxDataContainer(C, Kind::Module), ImageBytes(ImageByteMap::Create(C)) {}

Module::Module(Context& C, const std::string& X)
    : AuxDataContainer(C, Kind::Module), Name(X),
      ImageBytes(ImageByteMap::Create(C)) {}

gtirb::ImageByteMap& Module::getImageByteMap() { return *this->ImageBytes; }

const gtirb::ImageByteMap& Module::getImageByteMap() const {
  return *this->ImageBytes;
}

void Module::addCfgNode(CfgNode* N) {
  if (Block* B = dyn_cast<Block>(N))
    addBlock(B);
  else if (ProxyBlock* P = dyn_cast<ProxyBlock>(N))
    addProxyBlock(P);
  else
    assert("attempted to add invalid CfgNode");
}

void Module::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_binary_path(this->BinaryPath);
  Message->set_preferred_addr(static_cast<uint64_t>(this->PreferredAddr));
  Message->set_rebase_delta(this->RebaseDelta);
  Message->set_file_format(static_cast<proto::FileFormat>(this->FileFormat));
  Message->set_isa_id(static_cast<proto::ISAID>(this->IsaID));
  Message->set_name(this->Name);
  this->ImageBytes->toProtobuf(Message->mutable_image_byte_map());
  *Message->mutable_cfg() = gtirb::toProtobuf(this->Cfg);
  sequenceToProtobuf(block_begin(), block_end(), Message->mutable_blocks());
  sequenceToProtobuf(data_begin(), data_end(), Message->mutable_data());
  sequenceToProtobuf(ProxyBlocks.begin(), ProxyBlocks.end(),
                     Message->mutable_proxies());
  sequenceToProtobuf(section_begin(), section_end(),
                     Message->mutable_sections());
  containerToProtobuf(Symbols, Message->mutable_symbols());
  containerToProtobuf(SymbolicOperands, Message->mutable_symbolic_operands());
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
  setNodeUUIDFromBytes(M, Message.uuid());
  M->BinaryPath = Message.binary_path();
  M->PreferredAddr = Addr(Message.preferred_addr());
  M->RebaseDelta = Message.rebase_delta();
  M->FileFormat = static_cast<gtirb::FileFormat>(Message.file_format());
  M->IsaID = static_cast<ISAID>(Message.isa_id());
  M->Name = Message.name();
  M->ImageBytes = ImageByteMap::fromProtobuf(C, Message.image_byte_map());
  for (const auto& Elt : Message.blocks())
    M->addBlock(Block::fromProtobuf(C, Elt));
  for (const auto& Elt : Message.data())
    M->addData(DataObject::fromProtobuf(C, Elt));
  for (const auto& Elt : Message.proxies())
    M->addProxyBlock(ProxyBlock::fromProtobuf(C, Elt));
  for (const auto& Elt : Message.sections())
    M->addSection(Section::fromProtobuf(C, Elt));
  containerFromProtobuf(C, M->Symbols, Message.symbols());
  gtirb::fromProtobuf(C, M->Cfg, Message.cfg());
  // Create SymbolicExpressions after the Symbols they reference.
  containerFromProtobuf(C, M->SymbolicOperands, Message.symbolic_operands());
  AuxDataContainer::fromProtobuf(static_cast<AuxDataContainer*>(M), C,
                                 Message.aux_data_container());
  return M;
}
