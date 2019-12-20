//===- AuxData.cpp ---------------------------------------------*- C++-*-===//
//
//  Copyright (C) 2018-2019 GrammaTech, Inc.
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
#include <proto/AuxData.pb.h>

namespace gtirb {
void fromProtobuf(Context&, AuxData& Result, const proto::AuxData& Message) {
  Result.Impl = nullptr;
  Result.TypeName = Message.type_name();
  Result.RawBytes = Message.data();
}

proto::AuxData toProtobuf(const AuxData& T) {
  proto::AuxData Message;

  if (T.Impl != nullptr) {
    // Prefer the Impl, if it exists, since the user may have modified it after
    // deserialization.
    Message.set_type_name(T.Impl->typeName());
    Message.mutable_data()->clear();
    T.Impl->toBytes(*Message.mutable_data());
  } else if (!T.RawBytes.empty()) {
    // If there is no Impl, but RawBytes is not empty, the data must have been
    // deserialized from protobuf, but never used. So we can just put them back
    // into a protobuf.
    Message.set_type_name(T.TypeName);
    Message.set_data(T.RawBytes);
  }

  return Message;
}
} // namespace gtirb
