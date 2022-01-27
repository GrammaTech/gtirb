//===- test_floats.cpp ----------------------------------------------*- C++
//-*-===//
//
//  Copyright (C) 2021 GrammaTech, Inc.
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

#include <gtirb/gtirb.hpp>
#include <fstream>

struct AFloat {
  static constexpr const char* Name = "AFloat";
  typedef float Type;
};

struct ADouble {
  static constexpr const char* Name = "ADouble";
  typedef double Type;
};

void registerAuxData() {
  gtirb::AuxDataContainer::registerAuxDataType<AFloat>();
  gtirb::AuxDataContainer::registerAuxDataType<ADouble>();
};

int test_floats(const std::string& filename) {
  gtirb::Context C;
  std::ifstream inpt(filename, std::ios::binary);
  auto ir = gtirb::IR::load(C, inpt);
  if (ir) {
    auto f = (*ir)->getAuxData<AFloat>();
    auto g = (*ir)->getAuxData<ADouble>();
    if (f && g && (*f == 0.5) && (*g == 2.0)) {
      return 0;
    }
  }
  return 1;
}

void create_floats(const std::string& filename) {
  std::ofstream out(filename, std::ios::binary);
  gtirb::Context C;
  auto ir = gtirb::IR::Create(C);
  ir->addAuxData<AFloat>(0.5);
  ir->addAuxData<ADouble>(2.0);
  ir->save(out);
}

int main(int argc, char** argv) {
  if (argc < 2)
    return -1;
  registerAuxData();
  std::string filename;
  while (auto c = getopt(argc, argv, "r:w:")) {
    switch (c) {
    case 'w':
      create_floats(optarg);
      return 0;
    case 'r':
      return test_floats(optarg);
    default:
      break;
    }
  }
  return 0;
}
