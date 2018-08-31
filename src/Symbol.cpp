#include "Symbol.hpp"
#include "Serialization.hpp"
#include <gtirb/Block.hpp>
#include <gtirb/DataObject.hpp>

using namespace gtirb;

Symbol::Symbol(Addr X, std::string Name_, const DataObject& DO,
               StorageKind StorageKind_)
    : Symbol(X, Name_, StorageKind_) {
  this->setReferent(DO);
}

Symbol::Symbol(Addr X, std::string Name_, const Block& B,
               StorageKind StorageKind_)
    : Symbol(X, Name_, StorageKind_) {
  this->setReferent(B);
}

void Symbol::setReferent(const DataObject& Data) {
  this->Referent = NodeRef<DataObject>(Data);
}

void Symbol::setReferent(const Block& Instruction) {
  this->Referent = NodeRef<Block>(Instruction);
}

NodeRef<DataObject> Symbol::getDataReferent() const {
  auto *Ptr = std::get_if<NodeRef<DataObject>>(&this->Referent);
  return Ptr ? *Ptr : NodeRef<DataObject>();
}

NodeRef<Block> Symbol::getCodeReferent() const {
  auto *Ptr = std::get_if<NodeRef<Block>>(&this->Referent);
  return Ptr ? *Ptr : NodeRef<Block>();
}

void Symbol::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_address(static_cast<uint64_t>(this->Address));
  Message->set_name(this->Name);
  Message->set_storage_kind(static_cast<proto::StorageKind>(this->Storage));

  struct {
    MessageType &M;
  
    void operator()(const NodeRef<Block>& Arg) const {
      uuidToBytes(Arg.getUUID(), *M.mutable_code_referent_uuid());
    }
    void operator()(const NodeRef<DataObject>& Arg) const {
      uuidToBytes(Arg.getUUID(), *M.mutable_data_referent_uuid());
    }
  } Visitor{*Message};
  std::visit(Visitor, this->Referent);
}

Symbol* Symbol::fromProtobuf(Context& C, const MessageType& Message) {
  Symbol* S = Symbol::Create(C, Addr(Message.address()), Message.name(),
                             static_cast<StorageKind>(Message.storage_kind()));
  setNodeUUIDFromBytes(S, Message.uuid());
  if (!Message.code_referent_uuid().empty())
    S->Referent = NodeRef<Block>(uuidFromBytes(Message.code_referent_uuid()));
  else if (!Message.data_referent_uuid().empty())
    S->Referent =
        NodeRef<DataObject>(uuidFromBytes(Message.data_referent_uuid()));
  return S;
}
