#pragma once

#include <gtirb/Node.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/range/iterator_range.hpp>
#include <cstdint>
#include <gsl/gsl>
#include <memory>
#include <vector>

namespace proto {
class CFG;
}

namespace gtirb {
class Block;

///
/// Interprocedural control flow graph, with Blocks as the vertices.
///
using CFG = boost::adjacency_list<boost::setS,           // prevent parallel edges
                                  boost::vecS,           // preserve vertex order
                                  boost::bidirectionalS, // sucessor and predecessor edges
                                  Block>;                // vertices are blocks

template <typename Value, typename Graph>
class block_iter_base
    : public boost::iterator_facade<block_iter_base<Value, Graph>, Value,
                                    typename Graph::vertex_iterator::iterator_category> {
public:
  block_iter_base(typename Graph::vertex_iterator& it_, Graph& cfg_) : it(it_), cfg(&cfg_) {}

private:
  friend class boost::iterator_core_access;
  using self_type = block_iter_base<Value, Graph>;

  void increment() { ++(this->it); }

  void decrement() { this->it--; }

  void advance(int n) { this->it += 1; }

  typename Graph::vertex_iterator::difference_type distance_to(const self_type& other) const {
    return std::distance(this->it, other.it);
  }

  bool equal(const self_type& other) const { return this->it == other.it; }

  Value& dereference() const { return (*cfg)[*this->it]; }

  typename Graph::vertex_iterator it;
  gsl::not_null<Graph*> cfg;
};

using block_iterator = block_iter_base<Block, CFG>;
using const_block_iterator = block_iter_base<const Block, const CFG>;

///
/// Move a basic block into the graph.
///
/// \return a descriptor which can be used to retrieve the Block.
CFG::vertex_descriptor addBlock(CFG& cfg, Block&& block);

///
/// Iterates over Blocks in the graph.
///
boost::iterator_range<block_iterator> blocks(CFG& cfg);

///
/// Iterates over Blocks on the graph (by const reference).
///
boost::iterator_range<const_block_iterator> blocks(const CFG& cfg);

proto::CFG toProtobuf(const CFG& cfg);
void fromProtobuf(CFG& result, const proto::CFG& message);
} // namespace gtirb
