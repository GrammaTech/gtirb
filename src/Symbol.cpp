//===- Symbol.cpp -----------------------------------------------*- C++ -*-===//
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
#include "Symbol.hpp"
#include "IR.hpp"
#include "Serialization.hpp"
#include <gtirb/ByteInterval.hpp>
#include <gtirb/CodeBlock.hpp>
#include <gtirb/DataBlock.hpp>
#include <gtirb/proto/Symbol.pb.h>

using namespace gtirb;

class StorePayload {
public:
  StorePayload(proto::Symbol* Message) : M(Message) {}
  void operator()(std::monostate) const { M->clear_value(); }
  void operator()(Addr X) const { M->set_value(static_cast<uint64_t>(X)); }
  void operator()(const Node* Referent) const {
    nodeUUIDToBytes(Referent, *M->mutable_referent_uuid());
  }

private:
  proto::Symbol* M;
};

std::optional<Addr> Symbol::getAddress() const {
  return std::visit(
      [this](const auto& Arg) -> std::optional<Addr> {
        using T = std::decay_t<decltype(Arg)>;
        if constexpr (std::is_same_v<T, std::monostate>) {
          return std::nullopt;
        } else if constexpr (std::is_same_v<T, Addr>) {
          return Arg;
        } else if constexpr (std::is_same_v<T, Node*>) {
          if (auto* B = dyn_cast_or_null<CodeBlock>(Arg)) {
            if (auto A = B->getAddress()) {
              return *A + (AtEnd ? B->getSize() : 0);
            } else {
              return std::nullopt;
            }
          } else if (auto* D = dyn_cast_or_null<DataBlock>(Arg)) {
            if (auto A = D->getAddress()) {
              return *A + (AtEnd ? D->getSize() : 0);
            } else {
              return std::nullopt;
            }
          } else if (auto* P = dyn_cast_or_null<ProxyBlock>(Arg)) {
            return std::nullopt;
          } else {
            assert(Arg == nullptr && "unsupported referent type");
          }
          return std::nullopt;
        } else {
          static_assert(
              // Assert condition must depend on T, but will always be false.
              std::bool_constant<!std::is_same_v<T, T>>::value,
              "unsupported symbol payload type");
        }
      },
      Payload);
}

void Symbol::toProtobuf(MessageType* Message) const {
  nodeUUIDToBytes(this, *Message->mutable_uuid());
  std::visit(StorePayload(Message), Payload);
  Message->set_name(this->Name);
  Message->set_at_end(this->AtEnd);
}

ErrorOr<Symbol*> Symbol::fromProtobuf(Context& C, const MessageType& Message) {
  UUID Id;
  if (!uuidFromBytes(Message.uuid(), Id))
    return createStringError(IR::load_error::BadUUID, "could not load Symbol");

  Symbol* S = Symbol::Create(C, Message.name(), Message.at_end(), Id);

  switch (Message.optional_payload_case()) {
  case proto::Symbol::kValue: {
    S->Payload = Addr{Message.value()};
  } break;
  case proto::Symbol::kReferentUuid: {
    if (uuidFromBytes(Message.referent_uuid(), Id)) {
      if (auto* N = Node::getByUUID(C, Id)) {
        S->Payload = N;
      } else {
        S->Payload = std::monostate{};
      }
    } else {
      return nullptr;
    }
  } break;
  default:
      /* nothing to do */;
  }
  return S;
}

// Present for testing purposes only.
void Symbol::save(std::ostream& Out) const {
  MessageType Message;
  this->toProtobuf(&Message);
  Message.SerializeToOstream(&Out);
}

// Present for testing purposes only.
Symbol* Symbol::load(Context& C, std::istream& In) {
  MessageType Message;
  Message.ParseFromIstream(&In);
  auto S = Symbol::fromProtobuf(C, Message);
  if (S)
    return *S;
  return nullptr;
}
