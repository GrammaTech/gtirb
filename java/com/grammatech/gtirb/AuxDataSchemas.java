package com.grammatech.gtirb;

import com.grammatech.gtirb.auxdatacodec.*;
import java.util.*;

/**
 * This class contains schemas for sanctioned and provisional AuxData tables.
 *
 * Refer to AuxData.md for more information.
 */
public class AuxDataSchemas {

    /* Sanctioned AuxData tables. */

    /**
     * The CodeBlock to which a DT_INIT entry in an ELF file's .dynamic section
     * refers.
     */
    public final static AuxDataSchema<UUID> elfDynamicInit =
        new AuxDataSchema<>("elfDynamicInit", new UuidCodec());

    /**
     * The CodeBlock to which a DT_FINI entry in an ELF file's .dynamic section
     * refers.
     */
    public final static AuxDataSchema<UUID> elfDynamicFini =
        new AuxDataSchema<>("elfDynamicFini", new UuidCodec());

    /**
     * This table identifies all of the {@link CodeBlock}s that belong to each
     * function.
     */
    public final static AuxDataSchema<Map<UUID, Set<UUID>>> functionBlocks =
        new AuxDataSchema<>(
            "functionBlocks",
            new HashMapCodec<>(new UuidCodec(),
                               new HashSetCodec<>(new UuidCodec())));

    /**
     * This table identifies all {@link CodeBlock}s that represent entry points
     * to each function.
     */
    public final static AuxDataSchema<Map<UUID, Set<UUID>>> functionEntries =
        new AuxDataSchema<>(
            "functionEntries",
            new HashMapCodec<>(new UuidCodec(),
                               new HashSetCodec<>(new UuidCodec())));

    /**
     * This table identifies a canonical {@link Symbol} to be used for each
     * function.
     */
    public final static AuxDataSchema<Map<UUID, UUID>> functionNames =
        new AuxDataSchema<>(
            "functionNames",
            new HashMapCodec<>(new UuidCodec(), new UuidCodec()));

    /**
     * An entry in this table indicates that the given {@link DataBlock}
     * contains content that exhibits the given C++ type.
     */
    public final static AuxDataSchema<Map<UUID, String>> types =
        new AuxDataSchema<>(
            "types", new HashMapCodec<>(new UuidCodec(), new StringCodec()));

    /**
     * An entry in this table indicates that the given object's address is
     * required to be evenly divisible by the alignment value.
     */
    public final static AuxDataSchema<Map<UUID, Long>> alignment =
        new AuxDataSchema<>(
            "alignment", new HashMapCodec<>(new UuidCodec(), LongCodec.UINT64));

    /**
     * Comment strings relevant to offsets in the GTIRB entries.
     */
    public final static AuxDataSchema<Map<Offset, String>> comments =
        new AuxDataSchema<>("comments", new HashMapCodec<>(new OffsetCodec(),
                                                           new StringCodec()));

    /**
     * A mapping of symbols in one module that are bound dynamically to symbols
     * in another module.
     */
    public final static AuxDataSchema<Map<UUID, UUID>> symbolForwarding =
        new AuxDataSchema<>(
            "symbolForwarding",
            new HashMapCodec<>(new UuidCodec(), new UuidCodec()));

    /**
     * Locations of unused padding bytes in the binary.
     */
    public final static AuxDataSchema<Map<Offset, Long>> padding =
        new AuxDataSchema<>(
            "padding", new HashMapCodec<>(new OffsetCodec(), LongCodec.UINT64));

    /* Provisional AuxData tables. */

    /**
     * Type descriptors representing what type of binary the GTIRB models (e.g.
     * executable vs. dynamic library.)
     */
    public final static AuxDataSchema<List<String>> binaryType =
        new AuxDataSchema<>("binaryType",
                            new ArrayListCodec(new StringCodec()));

    /**
     * Map from Offsets to  vector of cfi directives.
     */
    public final static AuxDataSchema<Map<Offset, List<CfiDirective>>>
        cfiDirectives = new AuxDataSchema<>(
            "cfiDirectives",
            new HashMapCodec<>(
                new OffsetCodec(),
                new ArrayListCodec<>(new Tuple3Codec<>(
                    new StringCodec(), new ArrayListCodec<>(LongCodec.INT64),
                    new UuidCodec(), CfiDirective::new))));

    /**
     * Map from section UUIDs to tuples with the ELF section types and flags.
     */
    public final static AuxDataSchema<Map<UUID, ElfSectionPropertyTuple>>
        elfSectionProperties = new AuxDataSchema<>(
            "elfSectionProperties",
            new HashMapCodec<>(
                new UuidCodec(),
                new Tuple2Codec<>(LongCodec.UINT64, LongCodec.UINT64,
                                  ElfSectionPropertyTuple::new)));

    /**
     * Map from symbols to their type, binding, and visibility categories.
     */
    public final static AuxDataSchema<Map<UUID, ElfSymbolInfoTuple>>
        elfSymbolInfo = new AuxDataSchema<>(
            "elfSymbolInfo",
            new HashMapCodec<>(
                new UuidCodec(),
                new Tuple5Codec<>(LongCodec.UINT64, new StringCodec(),
                                  new StringCodec(), new StringCodec(),
                                  LongCodec.UINT64, ElfSymbolInfoTuple::new)));

    /**
     * Table of information about symbol versioning.
     */
    public final static AuxDataSchema<ElfSymbolVersionsTable>
        elfSymbolVersions = new AuxDataSchema<>(
            "elfSymbolVersions",
            new Tuple3Codec<>(
                new HashMapCodec<>(
                    ShortCodec.UINT16,
                    new Tuple2Codec<>(new ArrayListCodec<>(new StringCodec()),
                                      ShortCodec.UINT16,
                                      ElfSymbolVersionsTable.SymVerDef::new)),
                new HashMapCodec<>(
                    new StringCodec(),
                    new HashMapCodec<>(ShortCodec.UINT16, new StringCodec())),
                new HashMapCodec<>(
                    new UuidCodec(),
                    new Tuple2Codec<>(ShortCodec.UINT16, new BoolCodec(),
                                      ElfSymbolVersionsTable.SymVerEntry::new)),
                ElfSymbolVersionsTable::new));

    /**
     * Map from (typed) data objects to the encoding of the data, expressed as a
     * std::string containing an assembler encoding specifier: "string",
     * "uleb128" or "sleb128".
     */
    public final static AuxDataSchema<Map<UUID, String>> encodings =
        new AuxDataSchema<>("encodings", new HashMapCodec<>(new UuidCodec(),
                                                            new StringCodec()));

    /**
     * Map from function UUID to a list of weighted predictions.
     */
    public final static AuxDataSchema<
        Map<String, Map<UUID, List<ProbFuncName>>>> functionNameProbabilities =
        new AuxDataSchema<>(
            "functionNameProbabilities",
            new HashMapCodec<>(
                new StringCodec(),
                new HashMapCodec<>(new UuidCodec(),
                                   new ArrayListCodec<>(new Tuple3Codec<>(
                                       new StringCodec(), new StringCodec(),
                                       new FloatCodec(), ProbFuncName::new)))));

    /**
     * Names of libraries that are included in an executable (statically linked)
     */
    public final static AuxDataSchema<Map<UUID, String>> includedLibraryNames =
        new AuxDataSchema<>(
            "includedLibraryNames",
            new HashMapCodec<>(new UuidCodec(), new StringCodec()));

    /**
     * Versions of libraries that are included in an executable (statically
     * linked)
     */
    public final static AuxDataSchema<Map<UUID, String>>
        includedLibraryVersions = new AuxDataSchema<>(
            "includedLibraryVersions",
            new HashMapCodec<>(new UuidCodec(), new StringCodec()));

    /**
     * Names of the external libraries that are needed dynamically at run time.
     */
    public final static AuxDataSchema<List<String>> libraries =
        new AuxDataSchema<>("libraries",
                            new ArrayListCodec<>(new StringCodec()));

    /**
     * Paths contained in the rpath of the binary.
     */
    public final static AuxDataSchema<List<String>> libraryPaths =
        new AuxDataSchema<>("libraryPaths",
                            new ArrayListCodec<>(new StringCodec()));

    /**
     * List of tuples detailing an exported address, ordinal, and name for PE.
     */
    public final static AuxDataSchema<List<PeExportEntry>> peExportEntries =
        new AuxDataSchema<>("peExportEntries",
                            new ArrayListCodec<>(new Tuple3Codec<>(
                                LongCodec.UINT64, LongCodec.INT64,
                                new StringCodec(), PeExportEntry::new)));

    /**
     * UUIDs of the exported symbols for PE.
     */
    public final static AuxDataSchema<List<UUID>> peExportedSymbols =
        new AuxDataSchema<>("peExportedSymbols",
                            new ArrayListCodec<>(new UuidCodec()));

    /**
     * List of tuples detailing an imported function address, ordinal, function
     * name, and library names for PE.
     */
    public final static AuxDataSchema<List<PeImportEntry>> peImportEntries =
        new AuxDataSchema<>(
            "peImportEntries",
            new ArrayListCodec<>(new Tuple4Codec<>(
                LongCodec.UINT64, LongCodec.INT64, new StringCodec(),
                new StringCodec(), PeImportEntry::new)));

    /**
     * UUIDs of the imported symbols for PE.
     */
    public final static AuxDataSchema<List<UUID>> peImportedSymbols =
        new AuxDataSchema<>("peImportedSymbols",
                            new ArrayListCodec<>(new UuidCodec()));

    /**
     * List of PE resources.
     */
    public final static AuxDataSchema<List<PeResourceEntry>> peResource =
        new AuxDataSchema<>(
            "peResource",
            new ArrayListCodec<>(new Tuple3Codec<>(
                new ArrayListCodec<>(ByteCodec.UINT8), new OffsetCodec(),
                LongCodec.UINT64, PeResourceEntry::new)));

    /**
     * Profiling data. Executions by {@link CodeBlock}
     */
    public final static AuxDataSchema<Map<UUID, Long>> profile =
        new AuxDataSchema<>(
            "profile", new HashMapCodec<>(new UuidCodec(), LongCodec.UINT64));

    /**
     * Map of function UUIDs to their associated typeTable entries for the
     * purpose of giving them prototypes.
     */
    public final static AuxDataSchema<Map<UUID, UUID>> prototypeTable =
        new AuxDataSchema<>(
            "prototypeTable",
            new HashMapCodec<>(new UuidCodec(), new UuidCodec()));

    /**
     * The intra-procedural SCC identifier of each {@link CodeBlock}.
     */
    public final static AuxDataSchema<Map<UUID, Long>> sccs =
        new AuxDataSchema<>(
            "SCCs", new HashMapCodec<>(new UuidCodec(), LongCodec.INT64));

    /**
     * Map from an Offset of a {@link SymbolicExpression} in a {@link
     * ByteInterval} to its extent, a size in bytes.
     */
    public final static AuxDataSchema<Map<Offset, Long>>
        symbolicExpressionSizes = new AuxDataSchema<>(
            "symbolicExpressionSizes",
            new HashMapCodec<>(new OffsetCodec(), LongCodec.UINT64));

    /**
     * Structured type information about objects.
     */
    public final static AuxDataSchema<Map<UUID, TypeTableEntry>> typeTable =
        new AuxDataSchema<>(
            "typeTable",
            new HashMapCodec<>(
                new UuidCodec(),
                new Variant11Codec<>(
                    LongCodec.UINT64,
                    new Tuple1Codec<>(ByteCodec.UINT8,
                                      TypeTableEntry.BoolType::new),
                    new Tuple2Codec<>(ByteCodec.INT8, LongCodec.UINT64,
                                      TypeTableEntry.IntType::new),
                    LongCodec.UINT64, LongCodec.UINT64,
                    new Tuple2Codec<>(new UuidCodec(),
                                      new ArrayListCodec<>(new UuidCodec()),
                                      TypeTableEntry.FunctionType::new),
                    new UuidCodec(),
                    new Tuple2Codec<>(new UuidCodec(), LongCodec.UINT64,
                                      TypeTableEntry.ArrayType::new),
                    new UuidCodec(),
                    new Tuple2Codec<>(LongCodec.UINT64,
                                      new ArrayListCodec<>(new Tuple2Codec<>(
                                          LongCodec.UINT64, new UuidCodec(),
                                          TypeTableEntry.StructField::new)),
                                      TypeTableEntry.StructType::new),
                    new Tuple1Codec<>(ByteCodec.UINT8,
                                      TypeTableEntry.VoidType::new),
                    TypeTableEntry::makeUnknown, TypeTableEntry::makeBool,
                    TypeTableEntry::makeInt, TypeTableEntry::makeChar,
                    TypeTableEntry::makeFloat, TypeTableEntry::makeFunction,
                    TypeTableEntry::makePointer, TypeTableEntry::makeArray,
                    TypeTableEntry::makeAlias, TypeTableEntry::makeStruct,
                    TypeTableEntry::makeVoid)));
}
