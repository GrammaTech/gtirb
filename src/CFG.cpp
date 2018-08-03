#include "CFG.hpp"
#include "Serialization.hpp"
#include <gtirb/Block.hpp>
#include <proto/CFG.pb.h>
#include <map>

namespace gtirb {
CFG::vertex_descriptor addBlock(CFG& cfg, Block&& block) {
  auto descriptor = add_vertex(cfg);
  cfg[descriptor] = std::move(block);
  return descriptor;
}

boost::iterator_range<const_block_iterator> blocks(const CFG& cfg) {
  auto vs = vertices(cfg);
  return boost::iterator_range<const_block_iterator>(
      std::make_pair(const_block_iterator(vs.first, cfg), const_block_iterator(vs.second, cfg)));
}

boost::iterator_range<block_iterator> blocks(CFG& cfg) {
  auto vs = vertices(cfg);
  return boost::iterator_range<block_iterator>(
      std::make_pair(block_iterator(vs.first, cfg), block_iterator(vs.second, cfg)));
}

proto::CFG toProtobuf(const CFG& cfg) {
  proto::CFG message;
  containerToProtobuf(blocks(cfg), message.mutable_blocks());
  auto messageEdges = message.mutable_edges();
  auto edge_range = edges(cfg);
  std::for_each(edge_range.first, edge_range.second, [messageEdges, &cfg](const auto& e) {
    auto m = messageEdges->Add();
    nodeUUIDToBytes(&cfg[source(e, cfg)], *m->mutable_source_uuid());
    nodeUUIDToBytes(&cfg[target(e, cfg)], *m->mutable_target_uuid());
    auto label = cfg[e];
    switch (label.which()) {
    case 1:
      m->set_boolean(boost::get<bool>(label));
      break;
    case 2:
      m->set_integer(boost::get<uint64_t>(label));
      break;
    case 0:
    default:
      // Blank, nothing to do
      break;
    }
  });

  return message;
}

void fromProtobuf(CFG& result, const proto::CFG& message) {
  // While adding blocks, remember UUID -> vertex mapping
  std::map<UUID, CFG::vertex_descriptor> blockMap;
  std::for_each(message.blocks().begin(), message.blocks().end(),
                [&result, &blockMap](const auto& m) {
                  Block b;
                  fromProtobuf(b, m);
                  auto id = b.getUUID();
                  blockMap[id] = addBlock(result, std::move(b));
                });
  std::for_each(message.edges().begin(), message.edges().end(),
                [&result, &blockMap](const auto& m) {
                  auto e = add_edge(blockMap[uuidFromBytes(m.source_uuid())],
                                    blockMap[uuidFromBytes(m.target_uuid())], result)
                               .first;
                  switch (m.label_case()) {
                  case proto::Edge::kBoolean:
                    result[e] = m.boolean();
                    break;
                  case proto::Edge::kInteger:
                    result[e] = m.integer();
                  case proto::Edge::LABEL_NOT_SET:
                    // Nothing to do. Default edge label is blank.
                    break;
                  }
                });
}
} // namespace gtirb
