#pragma once

namespace gtirb
{
    ///
    /// \enum FileFormat
    ///
    enum class FileFormat
    {
        Undefined, /// Default value to indicates an uninitialized state.
        COFF,      /// Common Object File Format (COFF)
        ELF, /// Executable and Linkable Format (ELF, formerly named Extensible Linking Format)
        PE,  /// Microsoft Portable Executable (PE) format.
        IdaProDb32, /// IDA Pro database file
        IdaProDb64, /// IDA Pro database file
        XCOFF,      /// Non-COFF (files start with ANON_OBJECT_HEADER*)
        MACHO       /// Mach object file format
    };

    ///
    /// \enum FileFunction
    ///
    /// Also known as the file type.
    /// The name "FileType" was not used to avoid confusion with FileFormat.
    ///
    enum class FileFunction
    {
        Undefined, /// Default value to indicates an uninitialized state.
        Object,
        DynamicLibrary,
        Executable
    };

    ///
    /// \enum ISAID
    ///
    /// ISA ID
    ///
    enum class ISAID
    {
        Undefined, /// Default value to indicates an uninitialized state.
        IA32,      /// Intel Architecture, 32-bit. Also known as i386.
        PPC32, /// Performance Optimization With Enhanced RISC – Performance Computing, 32-bit.
        X64,   /// The generic name for the 64-bit extensions to both Intel's and AMD's 32-bit x86
               /// instruction set architecture (ISA).
        ARM,   /// Advanced RISC Machine. also known as Acorn RISC Machine.
        ValidButUnsupported
    };

    ///
    /// \enum OperandKind
    ///
    /// Instruction and Data Operand Type Information
    ///
    enum class OperandKind
    {
        Undefined,
        SymStackOffsetWConstant, /// Symbolic stack offset +/- Constant
        SymAddrGlobalWConstant,  /// Symbolic address (global) +/- Constant
        SymAddrSymAddr,          /// Symbolic address - Symbolic address
        SymAddrARMMov,           /// Symbolic address for ARM movw/movt/movl
    };
}