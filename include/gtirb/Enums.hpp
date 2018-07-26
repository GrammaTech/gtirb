#pragma once

#include <proto/Enums.pb.h>

namespace gtirb {
///
/// \enum FileFormat
///
enum class FileFormat : uint8_t {
  Undefined = proto::Format_Undefined, /// Default value to indicates an uninitialized state.
  COFF = proto::COFF,                  /// Common Object File Format (COFF)
  ELF = proto::ELF, /// Executable and Linkable Format (ELF, formerly named Extensible Linking
                    /// Format)
  PE = proto::PE,   /// Microsoft Portable Executable (PE) format.
  IdaProDb32 = proto::IdaProDb32, /// IDA Pro database file
  IdaProDb64 = proto::IdaProDb64, /// IDA Pro database file
  XCOFF = proto::XCOFF,           /// Non-COFF (files start with ANON_OBJECT_HEADER*)
  MACHO = proto::MACHO            /// Mach object file format
};

///
/// \enum FileFunction
///
/// Also known as the file type.
/// The name "FileType" was not used to avoid confusion with FileFormat.
///
enum class FileFunction : uint8_t {
  Undefined = proto::Function_Undefined, /// Default value to indicates an uninitialized state.
  Object = proto::Object,
  DynamicLibrary = proto::DynamicLibrary,
  Executable = proto::Executable
};

///
/// \enum ISAID
///
/// ISA ID
///
enum class ISAID : uint8_t {
  Undefined = proto::ISA_Undefined, /// Default value to indicates an uninitialized state.
  IA32 = proto::IA32,               /// Intel Architecture, 32-bit. Also known as i386.
  PPC32 = proto::PPC32,             /// Performance Optimization With Enhanced RISC – Performance
                                    /// Computing, 32-bit.
  X64 = proto::X64, /// The generic name for the 64-bit extensions to both Intel's and AMD's
                    /// 32-bit x86
                    /// instruction set architecture (ISA).
  ARM = proto::ARM, /// Advanced RISC Machine. also known as Acorn RISC Machine.
  ValidButUnsupported = proto::ValidButUnsupported
};
}
