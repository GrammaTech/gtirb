//===- Observer.hpp ---------------------------------------------*- C++ -*-===//
//
//  Copyright (C) 2020 GrammaTech, Inc.
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
#ifndef GTIRB_OBSERVER_H
#define GTIRB_OBSERVER_H

namespace gtirb {
enum class ChangeStatus {
  REJECTED, //< The requested change cannot be completed and must be rolled
            //< back.
  ACCEPTED, //< The requested change was implemented successfully.
  NO_CHANGE //< The requested change would not alter the data structure.
};
}

#endif // GTIRB_OBSERVER_H

