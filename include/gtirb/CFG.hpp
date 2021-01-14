//===- CFG.hpp --------------------------------------------------*- C++ -*-===//
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
// Traits for instantiating CfgEdgeIters as CfgPreds.
struct CfgPredTraits {
  typedef boost::graph_traits<CFG>::in_edge_iterator BGEdgeIter;
  static CfgNode* getNode(BGEdgeIter BGIt, const CFG& Cfg) {
    return Cfg[source(*BGIt, Cfg)];
  }
  static std::pair<BGEdgeIter, BGEdgeIter>
  getEdges(const CFG::vertex_descriptor& VtxDescr, const CFG& Cfg) {
    return in_edges(VtxDescr, Cfg);
  }
};
/// @endcond

/// @cond INTERNAL
// Traits for instantiating CfgEdgeIters as CfgSuccs.
struct CfgSuccTraits {
  typedef boost::graph_traits<CFG>::out_edge_iterator BGEdgeIter;
  static CfgNode* getNode(BGEdgeIter BGIt, const CFG& Cfg) {
    return Cfg[target(*BGIt, Cfg)];
  }
  static std::pair<BGEdgeIter, BGEdgeIter>
  getEdges(const CFG::vertex_descriptor& VtxDescr, const CFG& Cfg) {
    return out_edges(VtxDescr, Cfg);
  }
};
/// @endcond

/// @cond INTERNAL
// Template class underlying the CfgPreds and CfgSuccs helpers for iterating
// CFG predecessor and successor edges respectively from a given node.
// Each instance of a CfgEdgeIters object provides a container-like interface
// with a begin() and end() method for iterating the edges.  The iterator type
// is an internal Iter class whose value_type (type returned by dereference
// operator*) is a pair<CfgNode&, EdgeLabel>.
// Internally it stores a handle to the CFG and a pair of underlying
// boost::graph edge_iterators for begin and end.
template <class Traits> class CfgEdgeIters {
  // BG == "boost::graph"
  typedef typename Traits::BGEdgeIter BGEdgeIter;

public:
  // Iterator type returned by CfgEdgeIters's begin() and end() methods.
  // Its value_type (type returned by dereference operator*) is a
  // pair<CfgNode&, EdgeLabel>.
  // Internally it stores a handle to the CFG and the underlying boost::graph
  // edge_iterator.
  class Iter {
  public:
    using iterator_category = std::forward_iterator_tag;
    using value_type = std::pair<CfgNode&, EdgeLabel>;
    using difference_type = typename BGEdgeIter::difference_type;
    using pointer = value_type*;
    using reference = value_type&;

  private:
    const CFG& Cfg;
    BGEdgeIter BGIt;

  public:
    Iter(const Iter&) = default;
    Iter(const CFG& G, const BGEdgeIter& I) : Cfg(G), BGIt(I) {}

    value_type operator*() const {
      // NOTE: for some reason, std::make_pair does not compile.
      return value_type{*Traits::getNode(BGIt, Cfg), Cfg[*BGIt]};
    }
    bool operator==(const Iter& rhs) const { return BGIt == rhs.BGIt; }
    bool operator!=(const Iter& rhs) const { return BGIt != rhs.BGIt; }
    Iter& operator++() {
      ++BGIt;
      return *this;
    }
    Iter operator++(int) {
      auto Tmp = *this;
      ++BGIt;
      return Tmp;
    }
  };

private:
  const CFG& Cfg;
  const std::pair<BGEdgeIter, BGEdgeIter> BGEIters;

  // Helper method for construction
  static std::pair<BGEdgeIter, BGEdgeIter> initBGEIters(const CFG& G,
                                                        const CfgNode& N) {
    const std::optional<CFG::vertex_descriptor> OptVtxDescr = getVertex(&N, G);
    return (OptVtxDescr != std::nullopt)
               ? Traits::getEdges(OptVtxDescr.value(), G)
               : std::pair<BGEdgeIter, BGEdgeIter>();
  }

public:
  CfgEdgeIters(const CFG& G, const CfgNode& N)
      : Cfg(G), BGEIters(initBGEIters(G, N)) {}

  Iter begin() const { return Iter(Cfg, BGEIters.first); }
  Iter end() const { return Iter(Cfg, BGEIters.second); }
};
/// @endcond

/// \ingroup CFG_GROUP
/// \brief A container-like interface to iterate the predecessors
/// of a \ref CfgNode.
///
/// To iterate the predecessors of node N in graph G:
/// \code
/// for (const auto& [PredNode, EdgeLabel] : gtirb::CfgPreds(G, N)) { ... }
/// \endcode
typedef CfgEdgeIters<CfgPredTraits> CfgPreds;

/// \ingroup CFG_GROUP
/// \brief A container-like interface to iterate the successors
/// of a \ref CfgNode.
///
/// To iterate the successors of node N in graph G:
/// \code
/// for (const auto& [SuccNode, EdgeLabel] : gtirb::CfgSuccs(G, N)) { ... }
/// \endcode
typedef CfgEdgeIters<CfgSuccTraits> CfgSuccs;

} // namespace gtirb
#endif // GTIRB_CFG_H
