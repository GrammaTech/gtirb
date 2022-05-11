#include <gtirb/gtirb.hpp>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>
#include <iostream>
#include <map>
#include <string>
#include <variant>

struct SimpleVariantMap {
  typedef std::map<uint32_t, std::variant<int32_t, std::string>> Type;
  static constexpr const char* Name = "simpleVariantMap";
};

struct ComplexVariantMap {
  typedef std::map<std::string, std::variant<int64_t, std::string, int64_t>>
      Type;
  static constexpr const char* Name = "complexVariantMap";
};

static const SimpleVariantMap::Type simpleMap{
    {1, 1}, {2, "a"}, {3, "z"}, {4, -1}};

typedef std::variant<int64_t, std::string, int64_t> Var;

static const ComplexVariantMap::Type complexMap{
    {"a", Var(std::in_place_index<0>, 1)},
    {"b", Var("hello")},
    {"c", Var(std::in_place_index<2>, 4)},
    {"d", Var(std::in_place_index<0>, 0)}};

void register_schema() {
  gtirb::AuxDataContainer::registerAuxDataType<SimpleVariantMap>();
  gtirb::AuxDataContainer::registerAuxDataType<ComplexVariantMap>();
}

void add_auxdata(gtirb::IR* Ir) {
  auto NewSimpleMap = simpleMap;
  auto NewComplexMap = complexMap;
  Ir->addAuxData<SimpleVariantMap>(std::move(NewSimpleMap));
  Ir->addAuxData<ComplexVariantMap>(std::move(NewComplexMap));
}

namespace po = boost::program_options;
namespace fs = boost::filesystem;

bool write_ir(const std::string& filename) {
  std::ofstream dest{filename};
  gtirb::Context Ctx;
  auto* Ir = gtirb::IR::Create(Ctx);
  if (!Ir)
    return 1;
  add_auxdata(Ir);
  Ir->save(dest);
  return 0;
}

bool check_ir(const std::string& filename) {
  std::ifstream Src{filename, std::ios::binary | std::ios::in};
  gtirb::Context Ctx;
  auto MaybeIR = gtirb::IR::load(Ctx, Src);
  if (!MaybeIR)
    return 1;
  auto* Ir = *MaybeIR;
  auto* NewSimpleMap = Ir->getAuxData<SimpleVariantMap>();
  if (!NewSimpleMap)
    return 1;
  auto* NewComplexMap = Ir->getAuxData<ComplexVariantMap>();
  if (!NewComplexMap)
    return 1;
  return 0;
}

int main(int argc, char** argv) {
  register_schema();
  while (auto c = getopt(argc, argv, "r:w:")) {
    switch (c) {
    case 'r':
      return check_ir(optarg);
    case 'w':
      return write_ir(optarg);
    default:
      return 1;
    }
  }
  return 1;
}
