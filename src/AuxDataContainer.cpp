//===- AuxDataContainer.cpp -------------------------------------*- C++ -*-===//
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

#include "AuxDataContainer.hpp"
#include "AuxData.hpp"
#include "Context.hpp"
#include "Serialization.hpp"

#include <memory>
#include <string>

namespace gtirb {

struct AuxDataTypeMap {
  bool Locked = false;
  std::map<std::string, std::unique_ptr<AuxDataContainer::AuxDataType>> Map;
};

// Note: we are explicitly allocating the type map here w/ static
// storage to avoid having a variable symbol in the DLL interface on
// Windows. This allows us to not have to worry about dllimport'ing
// this symbol in client applications.
static AuxDataTypeMap TypeMap;

void AuxDataContainer::registerAuxDataTypeInternal(
    const char* Name, std::unique_ptr<AuxDataType> ADT) {
  assert(!TypeMap.Locked && "New AuxData types cannot be added at this point.");

  if (auto it = TypeMap.Map.find(Name); it != TypeMap.Map.end()) {
    // Failing this assertion indicates that two attempts to
    // register the same AuxData name are using different types.
    assert(it->second->getApiTypeId() == ADT->getApiTypeId() &&
           "Different types registered for the same AuxData name.");
    return;
  }

  TypeMap.Map.insert(std::make_pair(std::string(Name), std::move(ADT)));
}

void AuxDataContainer::checkAuxDataRegistration(
    const char* Name, [[maybe_unused]] std::size_t Id) {
  [[maybe_unused]] auto TypeEntry = TypeMap.Map.find(Name);
  assert(TypeEntry != TypeMap.Map.end() &&
         TypeEntry->second->getApiTypeId() == Id &&
         "Attempting to add AuxData with unregistered or incorrect type.");
}

const AuxDataContainer::AuxDataType*
AuxDataContainer::lookupAuxDataType(const std::string& Name) {
  if (auto It = TypeMap.Map.find(Name); It != TypeMap.Map.end()) {
    return It->second.get();
  }
  return nullptr;
}

AuxDataContainer::AuxDataContainer(Context& C, Node::Kind knd) : Node(C, knd) {
  // Once this is called, we outlaw registering new AuxData types.
  TypeMap.Locked = true;
}

AuxDataContainer::AuxDataContainer(Context& C, Node::Kind knd, const UUID& Uuid)
    : Node(C, knd, Uuid) {
  // Once this is called, we outlaw registering new AuxData types.
  TypeMap.Locked = true;
}

}; // namespace gtirb
