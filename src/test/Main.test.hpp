#ifndef MAIN_TEST_HPP
#define MAIN_TEST_HPP

namespace gtirb {
class IR;
};
// Utility for getting a handle to a previously build GTIRB IR.
const gtirb::IR* getTestIr();

#endif // MAIN_TEST_HPP
