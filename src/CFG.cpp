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
CFG::vertex_descriptor addVertex(Block* B, CFG& Cfg) {
  auto& IdTable = Cfg[boost::graph_bundle];
  auto it = IdTable.find(B);
  if (it != IdTable.end())
    return it->second;

  auto Vertex = add_vertex(Cfg);
  Cfg[Vertex] = B;
  IdTable[B] = Vertex;
  return Vertex;
}

std::optional<CFG::vertex_descriptor> getVertex(const Block* B,
                                                const CFG& Cfg) {
  auto& IdTable = Cfg[boost::graph_bundle];
  auto it = IdTable.find(B);
  if (it != IdTable.end())
    return it->second;
  return std::nullopt;
}

std::optional<CFG::edge_descriptor> addEdge(const Block* From, const Block* To,
                                            CFG& Cfg) {
  const auto& IdTable = Cfg[boost::graph_bundle];
  auto it = IdTable.find(cast<Node>(From));
  if (it == IdTable.end())
    return std::nullopt;
  auto FromVertex = it->second;

  it = IdTable.find(cast<Node>(To));
  if (it == IdTable.end())
    return std::nullopt;
  auto ToVertex = it->second;

  return add_edge(FromVertex, ToVertex, Cfg).first;
}

boost::iterator_range<const_block_iterator> blocks(const CFG& Cfg) {
  auto Vs = vertices(Cfg);
  return boost::make_iterator_range(
      const_block_iterator(Cfg, Vs.first, Vs.second),
      const_block_iterator(Cfg, Vs.second, Vs.second));
}

boost::iterator_range<block_iterator> blocks(CFG& Cfg) {
  auto Vs = vertices(Cfg);
  return boost::make_iterator_range(block_iterator(Cfg, Vs.first, Vs.second),
                                    block_iterator(Cfg, Vs.second, Vs.second));
}

proto::CFG toProtobuf(const CFG& Cfg) {
  proto::CFG Message;
  auto MessageVertices = Message.mutable_vertices();
  for (const Node& N : blocks(Cfg)) {
    auto* M = MessageVertices->Add();
    nodeUUIDToBytes(&N, *M);
  }

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
  for (const auto& M : Message.vertices()) {
    Block* B = dyn_cast_or_null<Block>(Node::getByUUID(C, uuidFromBytes(M)));
    addVertex(B, Result);
  }
  std::for_each(Message.edges().begin(), Message.edges().end(),
                [&Result, &C](const auto& M) {
                  auto* Source = dyn_cast_or_null<Block>(
                      Node::getByUUID(C, uuidFromBytes(M.source_uuid())));
                  auto* Target = dyn_cast_or_null<Block>(
                      Node::getByUUID(C, uuidFromBytes(M.target_uuid())));

                  if (Source && Target) {
                    auto E = addEdge(Source, Target, Result);
                    if (E) {
                      switch (M.label_case()) {
                      case proto::Edge::kBoolean:
                        Result[*E] = M.boolean();
                        break;
                      case proto::Edge::kInteger:
                        Result[*E] = M.integer();
                      case proto::Edge::LABEL_NOT_SET:
                        // Nothing to do. Default edge label is blank.
                        break;
                      }
                    }
                  }
                });
}
} // namespace gtirb
