//===- CFG.hpp --------------------------------------------------*- C++ -*-===//
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
#ifndef GTIRB_CFG_H
#define GTIRB_CFG_H

#include <gtirb/Casting.hpp>
#include <gtirb/Export.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/iterator/filter_iterator.hpp>
#include <boost/iterator/indirect_iterator.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include <boost/range/iterator_range.hpp>
#include <variant>

/// \file CFG.hpp
/// \ingroup CFG_GROUP
/// \brief Types and operations for interprocedural control flow
/// graphs (CFGs).
///
/// \see CFG_GROUP

namespace gtirb {
class CfgNode;
class CodeBlock;

/// \defgroup CFG_GROUP Control Flow Graphs (CFGs)
/// \brief Interprocedural control flow graph, with vertices of type
/// \ref Block.
///
/// See also \ref md_CFG-Edges.
///
/// @{ @}

/// \ingroup CFG_GROUP
/// \brief Indicates whether an edge is conditional on true.
enum class ConditionalEdge : bool {
  OnFalse, ///< \brief Indicates an unconditional edge or a conditional edge
           ///< that fires when the condition is false.
  OnTrue   ///< \brief Indicates a conditional edge that fires when the
           ///< condition is true.
};

/// \ingroup CFG_GROUP
/// \brief Indicates whether an edge represents indirect control flow.
enum class DirectEdge : bool { IsIndirect, IsDirect };

/// \ingroup CFG_GROUP
/// \brief Indicates the type of control flow transfer indicated by this edge.
enum class EdgeType { Branch, Call, Fallthrough, Return, Syscall, Sysret };

/// \ingroup CFG_GROUP
/// \brief A label on a \ref CFG edge.
using EdgeLabel =
    std::optional<std::tuple<ConditionalEdge, DirectEdge, EdgeType>>;

/// @cond INTERNAL

// Helper for constructing the CFG type. The graph property needs to refer to
// the graph's vertex_descriptor type. This is accessible via
// boost::adjacency_list_traits, but requires keeping the template parameters
// for boost::adjacency_list and boost::adjacency_list_traits in sync. This
// helper ensures the relevant parameters are the same for both.
template <class OutEdgeListS = boost::vecS, class VertexListS = boost::vecS,
          class DirectedS = boost::directedS, class EdgeListS = boost::listS>
struct CfgBuilder {
  using vertex_descriptor = typename boost::adjacency_list_traits<
      OutEdgeListS, VertexListS, DirectedS, EdgeListS>::vertex_descriptor;
  using type = boost::adjacency_list<
      OutEdgeListS, VertexListS, DirectedS,
      // Vertices are CfgNodes.
      CfgNode*,
      // Edges have labels.
      EdgeLabel,
      // The graph keeps track of vertex descriptors for
      // each node.
      std::unordered_map<const CfgNode*, vertex_descriptor>, EdgeListS>;
};
/// @endcond

/// \ingroup CFG_GROUP
/// \brief Interprocedural \ref CFG_GROUP "control flow graph", with
/// vertices of type \ref Block.
using CFG = CfgBuilder<boost::listS,         // allow parallel edges
                       boost::listS,         // preserve IDs after mutations
                       boost::bidirectionalS // successor and predecessor edges
                       >::type;
/// @cond INTERNAL
class cfg_node_iter_base
    : public boost::iterator_facade<cfg_node_iter_base,
                                    const boost::vertex_bundle_type<CFG>::type,
                                    CFG::vertex_iterator::iterator_category> {
public:
  cfg_node_iter_base() = default;
  cfg_node_iter_base(const CFG& cfg_, CFG::vertex_iterator& it_)
      : cfg(&cfg_), it(it_) {}

  // Use default move and copy constructors and assignment operators.
  cfg_node_iter_base(const cfg_node_iter_base&) = default;
  cfg_node_iter_base(cfg_node_iter_base&&) = default;
  cfg_node_iter_base& operator=(const cfg_node_iter_base&) = default;
  cfg_node_iter_base& operator=(cfg_node_iter_base&&) = default;

private:
  friend class boost::iterator_core_access;

  void increment() { ++it; }
  void decrement() { --it; }

  std::ptrdiff_t distance_to(const cfg_node_iter_base& other) const {
    return std::distance(it, other.it);
  }

  bool equal(const cfg_node_iter_base& other) const { return it == other.it; }

  const boost::vertex_bundle_type<CFG>::type& dereference() const {
    return (*cfg)[*it];
  }

  const CFG* cfg{nullptr};
  CFG::vertex_iterator it;
};

template <typename ToTy> struct downcast {
  template <typename FromTy> auto operator()(FromTy& Val) const {
    return dyn_cast_or_null<ToTy>(Val);
  }
};

struct not_null {
  template <typename T> bool operator()(const T* t) { return t != nullptr; }
};

template <typename T>
using cfg_node_downcast_iter =
    boost::transform_iterator<downcast<std::remove_const_t<T>>,
                              cfg_node_iter_base>;

template <typename T>
using cfg_node_downcast_not_null_iter =
    boost::filter_iterator<not_null, cfg_node_downcast_iter<T>>;

template <typename T>
class cfg_node_cast_iter
    : public boost::indirect_iterator<cfg_node_downcast_not_null_iter<T>, T> {
private:
  using xform_iterator = cfg_node_downcast_iter<T>;
  using filter_iterator = cfg_node_downcast_not_null_iter<T>;
  using parent = boost::indirect_iterator<filter_iterator, T>;

public:
  cfg_node_cast_iter() : parent() {}

  cfg_node_cast_iter(const CFG& g, CFG::vertex_iterator& first,
                     CFG::vertex_iterator& last)
      : parent(filter_iterator(xform_iterator(cfg_node_iter_base(g, first)),
                               xform_iterator(cfg_node_iter_base(g, last)))) {}

  template <typename OtherT>
  cfg_node_cast_iter(const cfg_node_cast_iter<OtherT>& other) : parent(other) {}
};

/// @endcond

/// \ingroup CFG_GROUP
/// \brief Iterator over CfgNodes (\ref CfgNode).
using cfg_iterator = boost::indirect_iterator<cfg_node_iter_base>;

/// \ingroup CFG_GROUP
/// \brief Const iterator over CfgNodes (\ref CfgNode).
using const_cfg_iterator =
    boost::indirect_iterator<cfg_node_iter_base, const CfgNode>;

/// \ingroup CFG_GROUP
/// \brief Iterator over blocks (\ref Block).
using block_iterator = cfg_node_cast_iter<CodeBlock>;

/// \ingroup CFG_GROUP
/// \brief Constant iterator over blocks (\ref Block).
using const_block_iterator = cfg_node_cast_iter<const CodeBlock>;

/// \ingroup CFG_GROUP
/// \brief Add a node to the CFG.
///
/// If the graph already contains the node, it is not modified.
///
/// \param N    The CFG node to add.
/// \param Cfg  The graph to modify.
///
/// \return A pair consisting of a descriptor to the vertex for that node and a
/// \c bool indicating whether the graph was modified.
GTIRB_EXPORT_API std::pair<CFG::vertex_descriptor, bool> addVertex(CfgNode* B,
                                                                   CFG& Cfg);

/// \ingroup CFG_GROUP
/// \brief Remove a node from the CFG.
///
/// If the graph does not contain the node, it is not modified.
///
/// \param N    The CFG node to remove.
/// \param Cfg  The graph to modify.
///
/// \return A \c bool indicating whether the graph was modified.
GTIRB_EXPORT_API bool removeVertex(CfgNode* N, CFG& Cfg);

/// \ingroup CFG_GROUP
/// \brief Get the boost::graph vertex descriptor for a CfgNode if it is in the
/// graph.
///
/// \param N    The node to query.
/// \param Cfg  The graph to query.
///
/// \return A descriptor which can be used to retrieve the node from the graph.
GTIRB_EXPORT_API std::optional<CFG::vertex_descriptor>
getVertex(const CfgNode* N, const CFG& Cfg);

/// \ingroup CFG_GROUP
/// \brief Create a new edge between two CFG nodes if they exist in the graph.
///
/// \param Cfg   The graph to modify.
/// \param From  The source node.
/// \param To    The target node.
///
/// \return A descriptor which can be used to retrieve the edge from the
/// graph or assign a label. If either CFG node is not present in the graph,
/// returns \c std::nullopt instead.
GTIRB_EXPORT_API std::optional<CFG::edge_descriptor>
addEdge(const CfgNode* From, const CfgNode* To, CFG& Cfg);

/// \ingroup CFG_GROUP
/// \brief Remove all edges between the source and target nodes from the CFG.
///
/// If the graph does not contain any of these nodes, it is not modified.
///
/// \param Cfg  The graph to modify.
/// \param From  The source node.
/// \param To    The target node.
///
/// \return A \c bool indicating whether the graph was modified.
GTIRB_EXPORT_API bool removeEdge(const CfgNode* From, const CfgNode* To,
                                 CFG& Cfg);

/// \ingroup CFG_GROUP
/// \brief Remove all edges with given label between the source and target nodes
/// from the CFG.
///
/// If the graph does not contain any of these nodes, it is not modified.
///
/// \param Cfg  The graph to modify.
/// \param From  The source node.
/// \param To    The target node.
/// \param Label The Edge label. Only edges with this label will be removed.
///
/// \return A \c bool indicating whether the graph was modified.
GTIRB_EXPORT_API bool removeEdge(const CfgNode* From, const CfgNode* To,
                                 const EdgeLabel Label, CFG& Cfg);

/// \ingroup CFG_GROUP
/// \brief Get a range of the \ref CfgNode elements in the specified graph.
///
/// \param Cfg  The graph to be iterated over.
///
/// \return a range over the \p Cfg.
GTIRB_EXPORT_API boost::iterator_range<cfg_iterator> nodes(CFG& Cfg);

/// \ingroup CFG_GROUP
/// \brief Get a constant range of the \ref CfgNode elements in the specified
/// graph.
///
/// \param Cfg  The graph to be iterated over.
///
/// \return A range over teh \p Cfg.
GTIRB_EXPORT_API boost::iterator_range<const_cfg_iterator>
nodes(const CFG& Cfg);

/// \ingroup CFG_GROUP
/// \brief Get a range of just the \ref Block elements in the specified graph.
///
/// The returned range will not include any \ref ProxyBlocks. To retrieve those
/// as well, use \ref nodes(CFG&).
///
/// \param Cfg  The graph to be iterated over.
///
/// \return A range over the \ref Blocks in the \p Cfg
GTIRB_EXPORT_API boost::iterator_range<block_iterator> blocks(CFG& Cfg);

/// \ingroup CFG_GROUP
/// \brief Get a constant range of just the \ref Block elements in the specified
/// graph.
///
/// The returned range will not include any \ref ProxyBlocks. To retrieve those
/// as well, use \ref nodes(CFG&).
///
/// \param Cfg  The graph to be iterated over.
///
/// \return A range over the \ref Blocks in the \p Cfg
GTIRB_EXPORT_API boost::iterator_range<const_block_iterator>
blocks(const CFG& Cfg);

/// @cond INTERNAL
// Traits for instantiating cfgEdgeIters as cfgPredecessors.
struct CfgPredecessorTraits {
  using edge_iterator = boost::graph_traits<CFG>::in_edge_iterator;
  static CfgNode* getNode(const CFG::edge_descriptor& EDesc, const CFG& Cfg) {
    return Cfg[source(EDesc, Cfg)];
  }
  static std::pair<edge_iterator, edge_iterator>
  getEdges(const CFG::vertex_descriptor& VtxDescr, const CFG& Cfg) {
    return in_edges(VtxDescr, Cfg);
  }
};
/// @endcond

/// @cond INTERNAL
// Traits for instantiating cfgEdgeIters as cfgSuccessors.
struct CfgSuccessorTraits {
  using edge_iterator = boost::graph_traits<CFG>::out_edge_iterator;
  static CfgNode* getNode(const CFG::edge_descriptor& EDesc, const CFG& Cfg) {
    return Cfg[target(EDesc, Cfg)];
  }
  static std::pair<edge_iterator, edge_iterator>
  getEdges(const CFG::vertex_descriptor& VtxDescr, const CFG& Cfg) {
    return out_edges(VtxDescr, Cfg);
  }
};
/// @endcond

/// @cond INTERNAL
/// \brief Convert a CFG edge_descriptor to a pair<[const] CfgNode*, EdgeLabel>.
///
/// \tparam Traits     Controls edge direction (predecessor vs successor).
/// \tparam CfgNodePtr Specifies constness of the returned CfgNode.
template <class Traits, typename CfgNodePtr> struct EdgeDescrToNodeLabel {
  // Yes, this stores a const CFG* even for the version that returns a
  // non-const CfgNode.  We can do this because the constness of the two are
  // actually independent, though we maintain the illusion of their related
  // constness in the public interface functions cfgPredecessors and
  // cfgSuccessors.
  const CFG* Cfg = nullptr; // Should only be null for end iterator

  EdgeDescrToNodeLabel() = default;
  EdgeDescrToNodeLabel(const CFG* G) : Cfg(G) {}
  // This quasi-copy constructor enables conversion from non-const iterator to
  // const iterator, but not vice versa.
  EdgeDescrToNodeLabel(const EdgeDescrToNodeLabel<Traits, CfgNode*>& Rhs)
      : Cfg(Rhs.Cfg) {}

  std::pair<CfgNodePtr, EdgeLabel>
  operator()(const CFG::edge_descriptor& EDesc) const {
    return {Traits::getNode(EDesc, *Cfg), (*Cfg)[EDesc]};
  }
};
/// @endcond

/// @cond INTERNAL
/// \brief Returns a iterator_range to iterate the predecessors or successors
/// of a \ref CfgNode.
///
/// This template method is instantiated as cfgPredecessors and cfgSuccessors
/// for iterating CFG predecessor and successor edges respectively from a given
/// node.  The underlying iterator's value_type (type returned by dereference
/// operator*) is a pair<[const] CfgNode*, EdgeLabel>.
///
/// \tparam Traits     Controls edge direction (predecessor vs successor).
/// \tparam CfgNodePtr Specifies constness of the returned CfgNode.
/// \param G  The \ref CFG containing N.
/// \param N  The \ref CfgNode whose edges will be iterated.
/// \return A range over \p N's predecessors or successors.
template <class Traits, typename CfgNodePtr>
auto cfgEdgeIters(const CFG& G, const CfgNode* N) {
  const std::optional<CFG::vertex_descriptor> OptVtxDescr = getVertex(N, G);
  if (OptVtxDescr == std::nullopt) {
    // FYI: this is the return type
    return boost::iterator_range<
        boost::transform_iterator<EdgeDescrToNodeLabel<Traits, CfgNodePtr>,
                                  typename Traits::edge_iterator>>();
  } else {
    const auto [Begin, End] = Traits::getEdges(OptVtxDescr.value(), G);
    return boost::make_iterator_range(
        boost::make_transform_iterator(
            Begin, EdgeDescrToNodeLabel<Traits, CfgNodePtr>{&G}),
        boost::make_transform_iterator(
            End, EdgeDescrToNodeLabel<Traits, CfgNodePtr>{&G}));
  }
}
/// @endcond

/// \brief iterator_range over a CfgNode's predecessors, non-const version
using cfg_predecessors_range = boost::iterator_range<boost::transform_iterator<
    EdgeDescrToNodeLabel<CfgPredecessorTraits, CfgNode*>,
    CfgPredecessorTraits::edge_iterator>>;
/// \brief iterator_range over a CfgNode's predecessors, const version
using const_cfg_predecessors_range =
    boost::iterator_range<boost::transform_iterator<
        EdgeDescrToNodeLabel<CfgPredecessorTraits, const CfgNode*>,
        CfgPredecessorTraits::edge_iterator>>;
/// \brief iterator_range over a CfgNode's successors, non-const version
using cfg_successors_range = boost::iterator_range<boost::transform_iterator<
    EdgeDescrToNodeLabel<CfgSuccessorTraits, CfgNode*>,
    CfgSuccessorTraits::edge_iterator>>;
/// \brief iterator_range over a CfgNode's successors, const version
using const_cfg_successors_range =
    boost::iterator_range<boost::transform_iterator<
        EdgeDescrToNodeLabel<CfgSuccessorTraits, const CfgNode*>,
        CfgSuccessorTraits::edge_iterator>>;

/// \ingroup CFG_GROUP
/// \brief Returns an iterator_range to iterate the predecessors
/// of a \ref CfgNode.
///
/// To iterate the predecessors of node N in graph G:
/// \code
/// for (auto [PredNode, EdgeLabel] : gtirb::cfgPredecessors(G, N)) { ... }
/// \endcode
///
/// \param G  The \ref CFG containing N.
/// \param N  The \ref CfgNode whose predecessors will be iterated.
/// \return A range over \p N's predecessors.
inline cfg_predecessors_range cfgPredecessors(CFG& G, const CfgNode* N) {
  return cfgEdgeIters<CfgPredecessorTraits, CfgNode*>(G, N);
}
inline const_cfg_predecessors_range cfgPredecessors(const CFG& G,
                                                    const CfgNode* N) {
  return cfgEdgeIters<CfgPredecessorTraits, const CfgNode*>(G, N);
}

/// \ingroup CFG_GROUP
/// \brief Returns an iterator_range to iterate the successors
/// of a \ref CfgNode.
///
/// To iterate the successors of node N in graph G:
/// \code
/// for (auto [SuccNode, EdgeLabel] : gtirb::cfgSuccessors(G, N)) { ... }
/// \endcode
///
/// \param G  The \ref CFG containing N.
/// \param N  The \ref CfgNode whose successors will be iterated.
/// \return A range over \p N's successors.
inline cfg_successors_range cfgSuccessors(CFG& G, const CfgNode* N) {
  return cfgEdgeIters<CfgSuccessorTraits, CfgNode*>(G, N);
}
inline const_cfg_successors_range cfgSuccessors(const CFG& G,
                                                const CfgNode* N) {
  return cfgEdgeIters<CfgSuccessorTraits, const CfgNode*>(G, N);
}

} // namespace gtirb
#endif // GTIRB_CFG_H
