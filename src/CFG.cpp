#include "CFG.hpp"
#include "Serialization.hpp"
#include <gtirb/Block.hpp>
#include <proto/CFG.pb.h>
#include <map>

namespace gtirb {
CFG::vertex_descriptor addBlock(CFG& Cfg, Block* B) {
  auto Descriptor = add_vertex(Cfg);
  Cfg[Descriptor] = B;
  return Descriptor;
}

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
  // While adding blocks, remember UUID -> vertex mapping
  std::map<UUID, CFG::vertex_descriptor> BlockMap;
  std::for_each(Message.blocks().begin(), Message.blocks().end(),
                [&Result, &BlockMap, &C](const auto& M) {
                  Block* B = Block::fromProtobuf(C, M);
                  auto Id = B->getUUID();
                  BlockMap[Id] = addBlock(Result, B);
                });
  std::for_each(Message.edges().begin(), Message.edges().end(),
                [&Result, &BlockMap](const auto& M) {
                  auto E =
                      add_edge(BlockMap[uuidFromBytes(M.source_uuid())],
                               BlockMap[uuidFromBytes(M.target_uuid())], Result)
                          .first;
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
                });
}
} // namespace gtirb
