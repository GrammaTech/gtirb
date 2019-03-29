//===- CFG.hpp --------------------------------------------------*- C++ -*-===//
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
#ifndef GTIRB_CFG_H
#define GTIRB_CFG_H

#include <gtirb/Context.hpp>
#include <gtirb/Export.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/range/iterator_range.hpp>
#include <gsl/gsl>
#include <variant>

/// \file CFG.hpp
/// \ingroup CFG_GROUP
/// \brief Types and operations for interprocedural control flow
/// graphs (CFGs).
///
/// \see CFG_GROUP

namespace proto {
class CFG;
}

namespace gtirb {
class Block;

/// \defgroup CFG_GROUP Control Flow Graphs (CFGs)
/// \brief Interprocedural control flow graph, with vertices of type
/// \ref Block.
///
/// See also \ref md_CFG-Edges.
///
/// @{ @}

/// \ingroup CFG_GROUP
/// \brief A label on a \ref CFG edge.
///
/// Boolean labels are used in the case of a conditional branch or call.
/// A true label designates the non-local edge (e.g. when the condition is
/// true), while a false label designates the fallthrough edge.
///
/// Integer labels are used for indirect branches.
using EdgeLabel = std::variant<std::monostate, bool, uint64_t>;

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
  using type =
      boost::adjacency_list<OutEdgeListS, VertexListS, DirectedS,
                            // Vertices are blocks.
                            Block*,
                            // Edges have labels.
                            EdgeLabel,
                            // The graph keeps track of vertex descriptors for
                            // each node.
                            std::unordered_map<const Node*, vertex_descriptor>,
                            EdgeListS>;
};
/// @endcond

/// \ingroup CFG_GROUP
/// \brief Interprocedural \ref CFG_GROUP "control flow graph", with
/// vertices of type \ref Block.
using CFG = CfgBuilder<boost::listS,         // allow parallel edges
                       boost::vecS,          // preserve vertex order
                       boost::bidirectionalS // successor and predecessor edges
                       >::type;
/// @cond INTERNAL
template <typename Value, typename Graph>
class block_iter_base
    : public boost::iterator_facade<
          block_iter_base<Value, Graph>, Value,
          typename Graph::vertex_iterator::iterator_category> {
public:
  block_iter_base(typename Graph::vertex_iterator& it_, Graph& cfg_)
      : it(it_), cfg(&cfg_) {}

private:
  friend class boost::iterator_core_access;
  using self_type = block_iter_base<Value, Graph>;

  void increment() { ++(this->it); }

  void decrement() { this->it--; }

  void advance(int n) { this->it += n; }

  typename Graph::vertex_iterator::difference_type
  distance_to(const self_type& other) const {
    return std::distance(this->it, other.it);
  }

  bool equal(const self_type& other) const { return this->it == other.it; }

  Value& dereference() const { return *(*cfg)[*this->it]; }

  typename Graph::vertex_iterator it;
  gsl::not_null<Graph*> cfg;
};
/// @endcond

/// \ingroup CFG_GROUP
/// \brief Iterator over blocks (\ref Block).
using block_iterator = block_iter_base<Block, CFG>;

/// \ingroup CFG_GROUP
/// \brief Constant iterator over blocks (\ref Block).
using const_block_iterator = block_iter_base<const Block, const CFG>;

/// \ingroup CFG_GROUP
/// \brief Add a block to the CFG.
///
/// If the graph already contains the block, it is not modified.
///
/// \warning This is a relatively low-level interface. For most purposes, prefer
/// Module::addBlock.
///
/// \param B    The block to add.
/// \param Cfg  The graph to modify.
///
/// \return A descriptor which can be used to retrieve the block from the graph.
GTIRB_EXPORT_API CFG::vertex_descriptor addVertex(Block* B, CFG& Cfg);

/// \ingroup CFG_GROUP
/// \brief Get the boost::graph vertex descriptor for a Block if it is in the
/// graph.
///
/// \param B    The block to query.
/// \param Cfg  The graph to query.
///
/// \return A descriptor which can be used to retrieve the block from the graph.
GTIRB_EXPORT_API std::optional<CFG::vertex_descriptor>
getVertex(const Block* B, const CFG& Cfg);

/// \ingroup CFG_GROUP
/// \brief Create a new edge between two blocks if they exist in the graph.
///
/// \param Cfg   The graph to modify.
/// \param From  The source block.
/// \param To    The target block.
///
/// \return A descriptor which can be used to retrieve the edge from the
/// graph or assign a label. If either block is not present in the graph,
/// returns \c std::nullopt instead.
GTIRB_EXPORT_API std::optional<CFG::edge_descriptor>
addEdge(const Block* From, const Block* To, CFG& Cfg);

/// \ingroup CFG_GROUP
/// \brief Get a range of the \ref Block elements in the specified graph.
///
/// \param Cfg  The graph to be iterated over.
///
/// \return A range over \p Cfg
GTIRB_EXPORT_API boost::iterator_range<block_iterator> blocks(CFG& Cfg);

/// \ingroup CFG_GROUP
/// \brief Get a constant range of the \ref Block elements in the specified
/// graph.
///
/// \param Cfg  The graph to be iterated over.
///
/// \return A range over \p Cfg
GTIRB_EXPORT_API boost::iterator_range<const_block_iterator>
blocks(const CFG& Cfg);

/// \ingroup CFG_GROUP
/// \brief Serialize a \ref CFG into a protobuf message.
///
/// \param Cfg   The CFG to serialize.
///
/// \return A protobuf message representing the \ref CFG and its
/// component blocks (\ref Block).
GTIRB_EXPORT_API proto::CFG toProtobuf(const CFG& Cfg);

/// \ingroup CFG_GROUP
/// \brief Initialize a \ref CFG from a protobuf message.
///
/// \param      C        The Context in which the deserialized CFG will be held.
/// \param      Message  The protobuf message from which to deserialize.
/// \param[out] Result   The CFG to initialize.
///
/// \return void
GTIRB_EXPORT_API void fromProtobuf(Context& C, CFG& Result,
                                   const proto::CFG& Message);
} // namespace gtirb

#endif // GTIRB_CFG_H
