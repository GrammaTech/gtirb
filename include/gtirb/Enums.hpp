#pragma once

namespace gtirb
{
	
///
/// \enum FileFormat
///
enum class FileFormat
{
    Undefined,
    COFF,
    ELF,
    PE,
    IdaProDb32, /// IDA Pro database file
    IdaProDb64, /// IDA Pro database file
    XCOFF, /// Non-COFF (files start with ANON_OBJECT_HEADER*)
    MACHO
};

///
/// \enum FileFunction
///
/// Also known as the file type.
/// "File Type" was not used to avoid confusion with FileFormat.
///
enum class FileFunction
{
    Undefined,
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
    Undefined,
    IA32,
    PPC32,
    X64,
    ARM,
    ValidButUnsupported
};

}