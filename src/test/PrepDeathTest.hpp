#ifndef PREP_DEATH_TEST_HPP
#define PREP_DEATH_TEST_HPP

#include "config-test.h"

#ifdef HAVE_RESOURCE_H
#include <cstdlib>
#include <cstring>
#include <sys/resource.h>

// Constructing an instance of this class will inhibit dumping of a
// core file on systems that support get/setrlimit(). This is useful
// to avoid generating core files when running death tests (tests that
// are designed to trigger failures - particularly assertions.) On
// destruction, the previous rlimit for core files is restored.
//
// One can set the env variable DEATH_TEST_CORE to a non-zero value to
// override this and still generate a core.
class PrepDeathTest {
public:
  PrepDeathTest() {
    char* EnvVar = getenv("DEATH_TEST_CORE");
    if (EnvVar == nullptr || strcmp(EnvVar, "0") == 0) {
      struct rlimit CurrCoreRLimit;
      if (getrlimit(RLIMIT_CORE, &CurrCoreRLimit) != 0) {
        // If this fails, just drop out and don't worry about
        // it. We'll just have to deal with extraneous cores.
        return;
      }

      PrevCoreRLimit = CurrCoreRLimit;
      CurrCoreRLimit.rlim_cur = 0;
      if (setrlimit(RLIMIT_CORE, &CurrCoreRLimit) != 0) {
        // If this fails, just drop out and don't worry about
        // it. We'll just have to deal with extraneous cores.
        return;
      }

      RLimitSet = true;
    }
  }

  ~PrepDeathTest() {
    if (RLimitSet) {
      setrlimit(RLIMIT_CORE, &PrevCoreRLimit);
    }
  }

private:
  bool RLimitSet = false;
  rlimit PrevCoreRLimit;
};

#else // HAVE_RESOURCE_H

// When we're not on a system that supports get/setrlimit(),
// PrepDeathTest is just a nop.
class PrepDeathTest {};

#endif // HAVE_RESOURCE_H

#endif // PREP_DEATH_TEST_HPP
