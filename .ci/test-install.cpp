#include <gtirb/gtirb.hpp>
#include <cstdio>
#include <fstream>

int main() {
  auto filename = std::tmpnam(nullptr);
  std::ofstream ofs{filename, std::ios_base::binary};
  auto ctx1 = gtirb::Context();
  auto ir1 = gtirb::IR::Create(ctx1);
  ir1->save(ofs);
  ofs.close();
  std::ifstream ifs{filename, std::ios_base::binary};
  auto ctx2 = gtirb::Context();
  if (auto ir2 = gtirb::IR::load(ctx2, ifs)) {

    return ir2.get()->modules().empty() ? 0 : 1;
  };
}
