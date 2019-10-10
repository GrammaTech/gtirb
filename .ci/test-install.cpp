#include <gtirb/IR.hpp>

int main() {
  auto ctx = gtirb::Context();
  auto ir = gtirb::IR::Create(ctx);
  return ir->modules().empty() ? 0 : 1;
}
