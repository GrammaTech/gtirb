//===- Node.cpp -------------------------------------------------*- C++ -*-===//
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
#include "Node.hpp"
#include "gtirb/Module.hpp"
#include <boost/uuid/uuid_generators.hpp>

using namespace gtirb;

// TODO: accessing this object between threads requires synchronization.
static boost::uuids::random_generator UUIDGenerator;

Node::Node(Context& C, Kind Knd, const UUID& U) : K(Knd), Uuid(U), Ctx(&C) {
  Ctx->registerNode(Uuid, this);
}

Node::Node(Context& C, Kind Knd) : Node(C, Knd, UUIDGenerator()) {}

Node::~Node() noexcept { Ctx->unregisterNode(this); }
