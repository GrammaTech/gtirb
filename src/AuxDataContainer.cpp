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

bool AuxDataContainer::removeAuxData(const std::string& X) {
  return this->AuxDatas.erase(X) > 0;
}
