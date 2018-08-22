#include "Symbol.hpp"
#include "Serialization.hpp"
#include <gtirb/Block.hpp>
#include <gtirb/DataObject.hpp>

using namespace gtirb;

Symbol::Symbol(Addr X, std::string Name_, const DataObject& Referent,
               StorageKind StorageKind_)
    : Symbol(X, Name_, StorageKind_) {
  this->setReferent(Referent);
}

Symbol::Symbol(Addr X, std::string Name_, const Block& Referent,
               StorageKind StorageKind_)
    : Symbol(X, Name_, StorageKind_) {
  this->setReferent(Referent);
}

void Symbol::setReferent(const DataObject& Data) {
  this->Referent = NodeRef<DataObject>(Data);
}

void Symbol::setReferent(const Block& Instruction) {
  this->Referent = NodeRef<Block>(Instruction);
}

void Symbol::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  Message->set_address(static_cast<uint64_t>(this->Address));
  Message->set_name(this->Name);
  Message->set_storage_kind(static_cast<proto::StorageKind>(this->Storage));
  std::visit(
      [Message](auto& Arg) {
        using T = std::decay_t<decltype(Arg)>;
        if constexpr (std::is_same_v<T, NodeRef<Block>>) {
          uuidToBytes(Arg.getUUID(), *Message->mutable_code_referent_uuid());
        } else if constexpr (std::is_same_v<T, NodeRef<DataObject>>) {
          uuidToBytes(Arg.getUUID(), *Message->mutable_data_referent_uuid());
        } else {
          static_assert(false, "Unknown symbol referent");
        }
      },
      this->Referent);
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
