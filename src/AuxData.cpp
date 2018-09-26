//===- AuxData.cpp ---------------------------------------------*- C++-*-===//
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
#include "AuxData.hpp"
#include "Serialization.hpp"
#include "gtirb/Context.hpp"
#include <proto/AuxData.pb.h>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <variant>

namespace gtirb {
void fromProtobuf(Context&, AuxData& Result, const proto::AuxData& Message) {
  Result.Impl = nullptr;
  Result.TypeName = Message.type_name();
  Result.RawBytes = Message.data();
}

proto::AuxData toProtobuf(const AuxData& T) {
  proto::AuxData Message;

  if (T.Impl != nullptr) {
    Message.set_type_name(T.Impl->typeName());
    Message.mutable_data()->clear();
    T.Impl->toBytes(*Message.mutable_data());
  }

  return Message;
}
} // namespace gtirb
