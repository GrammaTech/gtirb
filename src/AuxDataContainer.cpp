//===- AuxDataContainer.cpp -------------------------------------*- C++ -*-===//
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

#include "AuxDataContainer.hpp"
#include "Serialization.hpp"

using namespace gtirb;

void AuxDataContainer::addAuxData(const std::string& Name, AuxData&& X) {
  this->AuxDatas[Name] = std::move(X);
}

gtirb::AuxData* AuxDataContainer::getAuxData(const std::string& X) {
  auto Found = this->AuxDatas.find(X);
  if (Found != std::end(this->AuxDatas)) {
    return &(Found->second);
  }

  return nullptr;
}

const gtirb::AuxData* AuxDataContainer::getAuxData(const std::string& X) const {
  auto Found = this->AuxDatas.find(X);
  if (Found != std::end(this->AuxDatas)) {
    return &(Found->second);
  }

  return nullptr;
}

template <typename T>
T* AuxDataContainer::getAuxData(const std::string& X) const {
  const gtirb::AuxData* auxData = getAuxData(X);

  return auxData ? auxData->get<T>() : nullptr;
}

template <typename T> T* AuxDataContainer::getAuxData(const std::string& X) {
  gtirb::AuxData* auxData = getAuxData(X);

  return auxData ? auxData->get<T>() : nullptr;
}

bool AuxDataContainer::removeAuxData(const std::string& X) {
  return this->AuxDatas.erase(X) > 0;
}

void AuxDataContainer::toProtobuf(MessageType* Message) const {
  containerToProtobuf(this->AuxDatas, Message->mutable_aux_data());
}

void AuxDataContainer::fromProtobuf(AuxDataContainer* in, Context& C,
                                    const MessageType& Message) {
  containerFromProtobuf(C, in->AuxDatas, Message.aux_data());
}
