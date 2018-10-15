// An example program which opens an IR and prints every control-flow path
// from some basic block to another basic block.

#include <gtirb/gtirb.hpp>
#include <fstream>
#include <iomanip>
#include <set>
#include <vector>

using namespace gtirb;

// Print Addrs in hex format
std::ostream& operator<<(std::ostream& Os, Addr A) {
  auto Flags = Os.flags();
  Os << "0x" << std::hex << std::setw(8) << std::setfill('0') << uint64_t(A);
  Os.flags(Flags);
  return Os;
}

// Depth-first search of a graph, printing all paths between two given vertices.
// boost::graph::depth_first_search is not a good fit for this, because we
// need to visit nodes multiple times (while still avoiding cycles). So we
// have to implement our own.
class PrintPathsVisitor {
public:
  using Vertex = CFG::vertex_descriptor;

  PrintPathsVisitor(const CFG& G, const Block& B)
      : Graph(G), Target(B.getVertex()) {}

  void visit(Vertex V) {
    // Mark as visited to avoid cycles
    Visited.insert(V);

    // At target, print the path
    if (V == Target) {
      for (auto U : Path) {
        std::cout << Graph[U]->getAddress() << ", ";
      }
      std::cout << Graph[Target]->getAddress() << "\n";
    } else {
      // Otherwise, extend the path and keep searching.
      Path.push_back(V);
      // Check all outgoing edges from this vertex
      auto [Begin, End] = out_edges(V, Graph);
      for (auto It = Begin; It != End; It++) {
        // If edge target has not been visited, do so now
        auto T = target(*It, Graph);
        if (Visited.find(T) == Visited.end()) {
          visit(T);
        }
      }
      Path.pop_back();
    }

    // Unmark the node so it can be visited again in other paths.
    Visited.erase(V);
  }

  const CFG& Graph;
  Vertex Target;
  std::vector<Vertex> Path;
  std::set<Vertex> Visited;
};

int main(int argc, char** argv) {
  // Create a context to manage memory for gtirb objects
  Context C;

  // Load the IR
  IR* I;

  assert(argc == 4);
  {
    std::ifstream in(argv[1]);
    I = IR::load(C, in);
  }

  // Addresses of source and target blocks
  Addr Source(std::stoul(argv[2], nullptr, 16));
  Addr Target(std::stoul(argv[3], nullptr, 16));

  // Search for the requested blocks in the first module
  const auto& Cfg = I->modules()[0].getCFG();
  auto Blocks = blocks(Cfg);
  const Block *SourceBlock, *TargetBlock;

  if (auto SourceIt = std::find_if(
          Blocks.begin(), Blocks.end(),
          [Source](const auto& B) { return B.getAddress() == Source; });
      SourceIt != Blocks.end()) {
    SourceBlock = &*SourceIt;
  } else {
    std::cerr << "No block at source address " << Source << "\n";
    exit(1);
  }

  if (auto TargetIt = std::find_if(
          Blocks.begin(), Blocks.end(),
          [Target](const auto& B) { return B.getAddress() == Target; });
      TargetIt != Blocks.end()) {
    TargetBlock = &*TargetIt;
  } else {
    std::cerr << "No block at target address " << Target << "\n";
    exit(1);
  }

  std::cout << "Paths from " << SourceBlock->getAddress() << " to "
            << TargetBlock->getAddress() << "\n";
  // Print paths
  PrintPathsVisitor(Cfg, *TargetBlock).visit(SourceBlock->getVertex());
}
