//===- CFG.cpp --------------------------------------------------*- C++ -*-===//
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
#include "CFG.hpp"
#include "Serialization.hpp"
#include <gtirb/CodeBlock.hpp>
#include <gtirb/proto/CFG.pb.h>
#include <map>

namespace gtirb {
CFG::vertex_descriptor addVertex(CfgNode* B, CFG& Cfg) {
  auto& IdTable = Cfg[boost::graph_bundle];
  if (auto it = IdTable.find(B); it != IdTable.end()) {
    return it->second;
  }

  auto Vertex = add_vertex(Cfg);
  Cfg[Vertex] = B;
  IdTable[B] = Vertex;
  return Vertex;
}

void removeVertex(CfgNode* N, CFG& Cfg) {
  auto& IdTable = Cfg[boost::graph_bundle];
  if (auto it = IdTable.find(N); it != IdTable.end()) {
    clear_vertex(it->second, Cfg);
    remove_vertex(it->second, Cfg);
    IdTable.erase(it);
  }
}

std::optional<CFG::vertex_descriptor> getVertex(const CfgNode* N,
                                                const CFG& Cfg) {
  auto& IdTable = Cfg[boost::graph_bundle];
  if (auto it = IdTable.find(N); it != IdTable.end()) {
    return it->second;
  }
  return std::nullopt;
}

std::optional<CFG::edge_descriptor> addEdge(const CfgNode* From,
                                            const CfgNode* To, CFG& Cfg) {
  const auto& IdTable = Cfg[boost::graph_bundle];
  if (auto it = IdTable.find(From); it != IdTable.end()) {
    auto FromVertex = it->second;
    if (it = IdTable.find(To); it != IdTable.end()) {
      auto ToVertex = it->second;
      return add_edge(FromVertex, ToVertex, Cfg).first;
    }
  }
  return std::nullopt;
}

boost::iterator_range<const_cfg_iterator> nodes(const CFG& Cfg) {
  auto Vs = vertices(Cfg);
  return boost::make_iterator_range(
      const_cfg_iterator(cfg_node_iter_base(Cfg, Vs.first)),
      const_cfg_iterator(cfg_node_iter_base(Cfg, Vs.second)));
}

boost::iterator_range<cfg_iterator> nodes(CFG& Cfg) {
  auto Vs = vertices(Cfg);
  return boost::make_iterator_range(
      cfg_iterator(cfg_node_iter_base(Cfg, Vs.first)),
      cfg_iterator(cfg_node_iter_base(Cfg, Vs.second)));
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
  for (const Node& N : nodes(Cfg)) {
    auto* M = MessageVertices->Add();
    nodeUUIDToBytes(&N, *M);
  }

  auto MessageEdges = Message.mutable_edges();
  for (const auto& E : boost::make_iterator_range(edges(Cfg))) {
    auto M = MessageEdges->Add();
    nodeUUIDToBytes(Cfg[source(E, Cfg)], *M->mutable_source_uuid());
    nodeUUIDToBytes(Cfg[target(E, Cfg)], *M->mutable_target_uuid());
    if (auto Label = Cfg[E]) {
      auto* L = M->mutable_label();
      L->set_conditional(std::get<ConditionalEdge>(*Label) ==
                         ConditionalEdge::OnTrue);
      L->set_direct(std::get<DirectEdge>(*Label) == DirectEdge::IsDirect);
      L->set_type(static_cast<proto::EdgeType>(std::get<EdgeType>(*Label)));
    }
  }
  return Message;
}

void fromProtobuf(Context& C, CFG& Result, const proto::CFG& Message) {
  for (const auto& M : Message.vertices()) {
    if (UUID Id; uuidFromBytes(M, Id)) {
      auto* N = dyn_cast_or_null<CfgNode>(Node::getByUUID(C, Id));
      assert(N && "CFG message contains vertex that is not a CfgNode!");
      addVertex(N, Result);
    }
  }
  for (const auto& M : Message.edges()) {
    CfgNode *Source = nullptr, *Target = nullptr;
    if (UUID Id; uuidFromBytes(M.source_uuid(), Id))
      Source = dyn_cast_or_null<CfgNode>(Node::getByUUID(C, Id));
    if (UUID Id; uuidFromBytes(M.target_uuid(), Id))
      Target = dyn_cast_or_null<CfgNode>(Node::getByUUID(C, Id));
    if (Source && Target) {
      if (auto E = addEdge(Source, Target, Result); E && M.has_label()) {
        auto& L = M.label();
        Result[*E] = std::make_tuple(L.conditional() ? ConditionalEdge::OnTrue
                                                     : ConditionalEdge::OnFalse,
                                     L.direct() ? DirectEdge::IsDirect
                                                : DirectEdge::IsIndirect,
                                     static_cast<EdgeType>(L.type()));
      }
    }
  }
}

// This function is defined here w/ GTIRB_EXPORT_API to provide a
// means for test code to directly invoke serialization routines on a
// CFG. This is a capability not supported for GTIRB clients, but must
// be made available to the testing system.
void GTIRB_EXPORT_API cfgSave(const CFG& Cfg, std::ostream& Out) {
  proto::CFG Message = toProtobuf(Cfg);
  Message.SerializeToOstream(&Out);
}

// This function is defined here w/ GTIRB_EXPORT_API to provide a
// means for test code to directly invoke serialization routines on a
// CFG. This is a capability not supported for GTIRB clients, but must
// be made available to the testing system.
void GTIRB_EXPORT_API cfgLoad(Context& C, CFG& Result, std::istream& In) {
  proto::CFG Message;
  Message.ParseFromIstream(&In);
  fromProtobuf(C, Result, Message);
}

} // namespace gtirb
