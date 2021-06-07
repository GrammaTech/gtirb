//===- CFG.cpp --------------------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2021 GrammaTech, Inc.
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
#include <tuple>

namespace gtirb {

GTIRB_EXPORT_API std::ostream& operator<<(std::ostream& os,
                                          const ConditionalEdge& CondEdge) {
  switch (CondEdge) {
  case ConditionalEdge::OnFalse:
    os << "OnFalse";
    break;
  case ConditionalEdge::OnTrue:
    os << "OnTrue";
    break;
  }
  return os;
}

GTIRB_EXPORT_API std::ostream& operator<<(std::ostream& OS,
                                          const EdgeType& EdType) {
  switch (EdType) {
  case EdgeType::Branch:
    os << "Branch";
    break;
  case EdgeType::Call:
    os << "Call";
    break;
  case EdgeType::Fallthrough:
    os << "Fallthrough";
    break;
  case EdgeType::Return:
    os << "Return";
    break;
  case EdgeType::Syscall:
    os << "Syscall";
    break;
  case EdgeType::Sysret:
    os << "Sysret";
    break;
  default:
    os << "EdgeType -- Not Printed";
  }
  return os;
}

GTIRB_EXPORT_API std::ostream& operator<<(std::ostream& os,
                                          const DirectEdge& DirEdge) {
  switch (DirEdge) {
  case DirectEdge::IsIndirect:
    os << "IsIndirect";
    break;
  case DirectEdge::IsDirect:
    os << "IsDirect";
    break;
  }
  return os;
}

GTIRB_EXPORT_API std::ostream& operator<<(std::ostream& os,
                                          const EdgeLabel& label) {
  if (!label) {
    os << "<no label>";
  } else {
    auto et = std::get<EdgeType>(*label);
    auto de = std::get<DirectEdge>(*label);
    auto ce = std::get<ConditionalEdge>(*label);
    os << "(" << ce << ", " << de << ", " << et << ")";
    return os;
  }
}

std::pair<CFG::vertex_descriptor, bool> addVertex(CfgNode* B, CFG& Cfg) {
  auto& IdTable = Cfg[boost::graph_bundle];
  if (auto it = IdTable.find(B); it != IdTable.end()) {
    return std::make_pair(it->second, false);
  }

  auto Vertex = add_vertex(Cfg);
  Cfg[Vertex] = B;
  IdTable[B] = Vertex;
  return std::make_pair(Vertex, true);
}

bool removeVertex(CfgNode* N, CFG& Cfg) {
  auto& IdTable = Cfg[boost::graph_bundle];
  if (auto it = IdTable.find(N); it != IdTable.end()) {
    clear_vertex(it->second, Cfg);
    remove_vertex(it->second, Cfg);
    IdTable.erase(it);
    return true;
  }
  return false;
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

bool removeEdge(const CfgNode* From, const CfgNode* To, CFG& Cfg) {
  const auto& IdTable = Cfg[boost::graph_bundle];
  if (auto it = IdTable.find(From); it != IdTable.end()) {
    auto FromVertex = it->second;
    if (it = IdTable.find(To); it != IdTable.end()) {
      auto ToVertex = it->second;
      remove_edge(FromVertex, ToVertex, Cfg);
      return true;
    }
  }
  return false;
}

bool removeEdge(const CfgNode* From, const CfgNode* To, const EdgeLabel Label,
                CFG& Cfg) {
  bool remove_called = true, deleted = false;
  const auto& IdTable = Cfg[boost::graph_bundle];
  boost::graph_traits<CFG>::out_edge_iterator ei, edge_end;
  if (auto it = IdTable.find(From); it != IdTable.end()) {
    auto FromVertex = it->second;
    if (it = IdTable.find(To); it != IdTable.end()) {
      while (remove_called) {
        remove_called = false;
        for (boost::tie(ei, edge_end) = out_edges(FromVertex, Cfg);
             ei != edge_end; ++ei) {
          if (Cfg[*ei] == Label) {
            remove_edge(ei, Cfg);
            // As remove_edge invalidate all iterators
            // the iteration process should be restarted
            remove_called = true;
            deleted = true;
            break;
          }
        }
      }
    }
  }
  return deleted;
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

bool fromProtobuf(Context& C, CFG& Result, const proto::CFG& Message) {
  // Because we're deserializing, we have to assume the data is attacker-
  // controlled and may be malicious. We cannot use cast<> because an attacker
  // could specify the UUID to a node of the incorrect type. Instead, we use
  // dyn_cast<> and assert as needed.
  for (const auto& M : Message.vertices()) {
    UUID Id;
    if (!uuidFromBytes(M, Id))
      return false;
    auto* N = dyn_cast_or_null<CfgNode>(Node::getByUUID(C, Id));
    assert(N && "CFG message contains vertex that is not a CfgNode!");
    if (!N)
      return false;
    addVertex(N, Result);
  }
  for (const auto& M : Message.edges()) {
    UUID Id;
    if (!uuidFromBytes(M.source_uuid(), Id))
      return false;
    CfgNode* Source = dyn_cast_or_null<CfgNode>(Node::getByUUID(C, Id));

    if (!uuidFromBytes(M.target_uuid(), Id))
      return false;
    CfgNode* Target = dyn_cast_or_null<CfgNode>(Node::getByUUID(C, Id));
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
  return true;
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
  (void)fromProtobuf(C, Result, Message);
}

} // namespace gtirb
