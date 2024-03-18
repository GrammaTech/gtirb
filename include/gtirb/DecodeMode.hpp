//===- DecodeMode.hpp --------------------------------------------*- C++-*-===//
//
//  Copyright (C) 2024 GrammaTech, Inc.
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
#ifndef GTIRB_DECODE_MODE_H
#define GTIRB_DECODE_MODE_H

#include <gtirb/proto/CodeBlock.pb.h>
#include <cstdint>

namespace gtirb {

/// \enum DecodeMode
///
/// \brief Variations on decoding a particular ISA
enum class DecodeMode : uint8_t {
  Default = proto::All_Default, ///< Default decode mode for all ISAs
  Thumb = proto::ARM_Thumb,     ///< Thumb decode mode for ARM32
};

} // namespace gtirb

#endif // GTIRB_DECODE_MODE_H
