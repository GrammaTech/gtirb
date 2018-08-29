#include "Symbol.hpp"
#include <gtirb/Addr.hpp>
#include <gtirb/SymbolSet.hpp>

namespace gtirb {

std::vector<const Symbol*> findSymbols(const SymbolSet& Symbols, Addr X) {
  auto Found = Symbols.equal_range(X);
  std::vector<const Symbol*> result;
  std::for_each(Found.first, Found.second, [&result](const auto& node) {
    result.push_back(node.second);
  });
  return result;
}

std::vector<Symbol*> findSymbols(SymbolSet& Symbols, Addr X) {
  auto Found = Symbols.equal_range(X);
  std::vector<Symbol*> Result;
  std::for_each(Found.first, Found.second,
                [&Result](auto& Node) { Result.push_back(Node.second); });
  return Result;
}
} // namespace gtirb
