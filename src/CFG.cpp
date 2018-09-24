//===- CFG.cpp --------------------------------------------------*- C++ -*-===//
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
#include "CFG.hpp"
#include "Serialization.hpp"
#include <gtirb/Block.hpp>
#include <proto/CFG.pb.h>
#include <map>

namespace gtirb {
CFG::edge_descriptor addEdge(const Block* From, const Block* To, CFG& Cfg) {
  return add_edge(From->getVertex(), To->getVertex(), Cfg).first;
} // namespace gtirb

boost::iterator_range<const_block_iterator> blocks(const CFG& Cfg) {
  auto Vs = vertices(Cfg);
  return boost::iterator_range<const_block_iterator>(
      std::make_pair(const_block_iterator(Vs.first, Cfg),
                     const_block_iterator(Vs.second, Cfg)));
}

boost::iterator_range<block_iterator> blocks(CFG& Cfg) {
  auto Vs = vertices(Cfg);
  return boost::iterator_range<block_iterator>(std::make_pair(
      block_iterator(Vs.first, Cfg), block_iterator(Vs.second, Cfg)));
}

proto::CFG toProtobuf(const CFG& Cfg) {
  proto::CFG Message;
  containerToProtobuf(blocks(Cfg), Message.mutable_blocks());
  auto MessageEdges = Message.mutable_edges();
  auto EdgeRange = edges(Cfg);
  std::for_each(
      EdgeRange.first, EdgeRange.second, [MessageEdges, &Cfg](const auto& E) {
        auto M = MessageEdges->Add();
        nodeUUIDToBytes(Cfg[source(E, Cfg)], *M->mutable_source_uuid());
        nodeUUIDToBytes(Cfg[target(E, Cfg)], *M->mutable_target_uuid());
        auto Label = Cfg[E];
        switch (Label.index()) {
        case 1:
          M->set_boolean(std::get<bool>(Label));
          break;
        case 2:
          M->set_integer(std::get<uint64_t>(Label));
          break;
        case 0:
        default:
          // Blank, nothing to do
          break;
        }
      });

  return Message;
}

void fromProtobuf(Context& C, CFG& Result, const proto::CFG& Message) {
  std::for_each(Message.blocks().begin(), Message.blocks().end(),
                [&Result, &C](const auto& M) {
                  auto* B =
                      emplaceBlock(Result, C, Addr(M.address()), M.size(),
                                   Block::Exit(M.exit_kind()), M.decode_mode());
                  setNodeUUIDFromBytes(B, M.uuid());
                });
  std::for_each(Message.edges().begin(), Message.edges().end(),
                [&Result, &C](const auto& M) {
                  auto* Source = dyn_cast_or_null<Block>(
                      Node::getByUUID(C, uuidFromBytes(M.source_uuid())));
                  auto* Target = dyn_cast_or_null<Block>(
                      Node::getByUUID(C, uuidFromBytes(M.target_uuid())));

                  if (Source && Target) {
                    auto E = addEdge(Source, Target, Result);
                    switch (M.label_case()) {
                    case proto::Edge::kBoolean:
                      Result[E] = M.boolean();
                      break;
                    case proto::Edge::kInteger:
                      Result[E] = M.integer();
                    case proto::Edge::LABEL_NOT_SET:
                      // Nothing to do. Default edge label is blank.
                      break;
                    }
                  }
                });
}
} // namespace gtirb
