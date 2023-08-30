package com.grammatech.gtirb;

import com.grammatech.gtirb.auxdatacodec.BoolCodec;
import com.grammatech.gtirb.auxdatacodec.ByteCodec;
import com.grammatech.gtirb.auxdatacodec.FloatCodec;
import com.grammatech.gtirb.auxdatacodec.ListCodec;
import com.grammatech.gtirb.auxdatacodec.LongCodec;
import com.grammatech.gtirb.auxdatacodec.MapCodec;
import com.grammatech.gtirb.auxdatacodec.OffsetCodec;
import com.grammatech.gtirb.auxdatacodec.SetCodec;
import com.grammatech.gtirb.auxdatacodec.ShortCodec;
import com.grammatech.gtirb.auxdatacodec.StringCodec;
import com.grammatech.gtirb.auxdatacodec.Tuple1Codec;
import com.grammatech.gtirb.auxdatacodec.Tuple2Codec;
import com.grammatech.gtirb.auxdatacodec.Tuple3Codec;
import com.grammatech.gtirb.auxdatacodec.Tuple4Codec;
import com.grammatech.gtirb.auxdatacodec.Tuple5Codec;
import com.grammatech.gtirb.auxdatacodec.UuidCodec;
import com.grammatech.gtirb.auxdatacodec.Variant11Codec;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

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
            new MapCodec<>(new UuidCodec(),
                           new SetCodec<>(new UuidCodec(), HashSet::new),
                           HashMap::new));

    /**
     * This table identifies all {@link CodeBlock}s that represent entry points
     * to each function.
     */
    public final static AuxDataSchema<Map<UUID, Set<UUID>>> functionEntries =
        new AuxDataSchema<>(
            "functionEntries",
            new MapCodec<>(new UuidCodec(),
                           new SetCodec<>(new UuidCodec(), HashSet::new),
                           HashMap::new));

    /**
     * This table identifies a canonical {@link Symbol} to be used for each
     * function.
     */
    public final static AuxDataSchema<Map<UUID, UUID>> functionNames =
        new AuxDataSchema<>(
            "functionNames",
            new MapCodec<>(new UuidCodec(), new UuidCodec(), HashMap::new));

    /**
     * An entry in this table indicates that the given {@link DataBlock}
     * contains content that exhibits the given C++ type.
     */
    public final static AuxDataSchema<Map<UUID, String>> types =
        new AuxDataSchema<>(
            "types",
            new MapCodec<>(new UuidCodec(), new StringCodec(), HashMap::new));

    /**
     * An entry in this table indicates that the given object's address is
     * required to be evenly divisible by the alignment value.
     */
    public final static AuxDataSchema<Map<UUID, Long>> alignment =
        new AuxDataSchema<>(
            "alignment",
            new MapCodec<>(new UuidCodec(), LongCodec.UINT64, HashMap::new));

    /**
     * Comment strings relevant to offsets in the GTIRB entries.
     */
    public final static AuxDataSchema<Map<Offset, String>> comments =
        new AuxDataSchema<>(
            "comments",
            new MapCodec<>(new OffsetCodec(), new StringCodec(), HashMap::new));

    /**
     * A mapping of symbols in one module that are bound dynamically to symbols
     * in another module.
     */
    public final static AuxDataSchema<Map<UUID, UUID>> symbolForwarding =
        new AuxDataSchema<>(
            "symbolForwarding",
            new MapCodec<>(new UuidCodec(), new UuidCodec(), HashMap::new));

    /**
     * Locations of unused padding bytes in the binary.
     */
    public final static AuxDataSchema<Map<Offset, Long>> padding =
        new AuxDataSchema<>(
            "padding",
            new MapCodec<>(new OffsetCodec(), LongCodec.UINT64, HashMap::new));

    /* Provisional AuxData tables. */

    /**
     * Type descriptors representing what type of binary the GTIRB models (e.g.
     * executable vs. dynamic library.)
     */
    public final static AuxDataSchema<List<String>> binaryType =
        new AuxDataSchema<>("binaryType",
                            new ListCodec(new StringCodec(), ArrayList::new));

    /**
     * Map from Offsets to  vector of cfi directives.
     */
    public final static AuxDataSchema<Map<Offset, List<CfiDirective>>>
        cfiDirectives = new AuxDataSchema<>(
            "cfiDirectives",
            new MapCodec<>(
                new OffsetCodec(),
                new ListCodec<>(
                    new Tuple3Codec<>(
                        new StringCodec(),
                        new ListCodec<>(LongCodec.INT64, ArrayList::new),
                        new UuidCodec(), CfiDirective::new),
                    ArrayList::new),
                HashMap::new));

    /**
     * Map from section UUIDs to tuples with the ELF section types and flags.
     */
    public final static AuxDataSchema<Map<UUID, ElfSectionPropertyTuple>>
        elfSectionProperties = new AuxDataSchema<>(
            "elfSectionProperties",
            new MapCodec<>(new UuidCodec(),
                           new Tuple2Codec<>(LongCodec.UINT64, LongCodec.UINT64,
                                             ElfSectionPropertyTuple::new),
                           HashMap::new));

    /**
     * Map from symbols to their type, binding, and visibility categories.
     */
    public final static AuxDataSchema<Map<UUID, ElfSymbolInfoTuple>>
        elfSymbolInfo = new AuxDataSchema<>(
            "elfSymbolInfo",
            new MapCodec<>(
                new UuidCodec(),
                new Tuple5Codec<>(LongCodec.UINT64, new StringCodec(),
                                  new StringCodec(), new StringCodec(),
                                  LongCodec.UINT64, ElfSymbolInfoTuple::new),
                HashMap::new));

    /**
     * Table of information about symbol versioning.
     */
    public final static AuxDataSchema<ElfSymbolVersionsTable>
        elfSymbolVersions = new AuxDataSchema<>(
            "elfSymbolVersions",
            new Tuple3Codec<>(
                new MapCodec<>(
                    ShortCodec.UINT16,
                    new Tuple2Codec<>(
                        new ListCodec<>(new StringCodec(), ArrayList::new),
                        ShortCodec.UINT16,
                        ElfSymbolVersionsTable.SymVerDef::new),
                    HashMap::new),
                new MapCodec<>(new StringCodec(),
                               new MapCodec<>(ShortCodec.UINT16,
                                              new StringCodec(), HashMap::new),
                               HashMap::new),
                new MapCodec<>(
                    new UuidCodec(),
                    new Tuple2Codec<>(ShortCodec.UINT16, new BoolCodec(),
                                      ElfSymbolVersionsTable.SymVerEntry::new),
                    HashMap::new),
                ElfSymbolVersionsTable::new));

    /**
     * Map from (typed) data objects to the encoding of the data, expressed as a
     * std::string containing an assembler encoding specifier: "string",
     * "uleb128" or "sleb128".
     */
    public final static AuxDataSchema<Map<UUID, String>> encodings =
        new AuxDataSchema<>(
            "encodings",
            new MapCodec<>(new UuidCodec(), new StringCodec(), HashMap::new));

    /**
     * Map from function UUID to a list of weighted predictions.
     */
    public final static AuxDataSchema<
        Map<String, Map<UUID, List<ProbFuncName>>>> functionNameProbabilities =
        new AuxDataSchema<>(
            "functionNameProbabilities",
            new MapCodec<>(
                new StringCodec(),
                new MapCodec<>(
                    new UuidCodec(),
                    new ListCodec<>(
                        new Tuple3Codec<>(new StringCodec(), new StringCodec(),
                                          new FloatCodec(), ProbFuncName::new),
                        ArrayList::new),
                    HashMap::new),
                HashMap::new));

    /**
     * Names of libraries that are included in an executable (statically linked)
     */
    public final static AuxDataSchema<Map<UUID, String>> includedLibraryNames =
        new AuxDataSchema<>(
            "includedLibraryNames",
            new MapCodec<>(new UuidCodec(), new StringCodec(), HashMap::new));

    /**
     * Versions of libraries that are included in an executable (statically
     * linked)
     */
    public final static AuxDataSchema<Map<UUID, String>>
        includedLibraryVersions = new AuxDataSchema<>(
            "includedLibraryVersions",
            new MapCodec<>(new UuidCodec(), new StringCodec(), HashMap::new));

    /**
     * Names of the external libraries that are needed dynamically at run time.
     */
    public final static AuxDataSchema<List<String>> libraries =
        new AuxDataSchema<>("libraries",
                            new ListCodec<>(new StringCodec(), ArrayList::new));

    /**
     * Paths contained in the rpath of the binary.
     */
    public final static AuxDataSchema<List<String>> libraryPaths =
        new AuxDataSchema<>("libraryPaths",
                            new ListCodec<>(new StringCodec(), ArrayList::new));

    /**
     * List of tuples detailing an exported address, ordinal, and name for PE.
     */
    public final static AuxDataSchema<List<PeExportEntry>> peExportEntries =
        new AuxDataSchema<>(
            "peExportEntries",
            new ListCodec<>(new Tuple3Codec<>(LongCodec.UINT64, LongCodec.INT64,
                                              new StringCodec(),
                                              PeExportEntry::new),
                            ArrayList::new));

    /**
     * UUIDs of the exported symbols for PE.
     */
    public final static AuxDataSchema<List<UUID>> peExportedSymbols =
        new AuxDataSchema<>("peExportedSymbols",
                            new ListCodec<>(new UuidCodec(), ArrayList::new));

    /**
     * List of tuples detailing an imported function address, ordinal, function
     * name, and library names for PE.
     */
    public final static AuxDataSchema<List<PeImportEntry>> peImportEntries =
        new AuxDataSchema<>(
            "peImportEntries",
            new ListCodec<>(new Tuple4Codec<>(LongCodec.UINT64, LongCodec.INT64,
                                              new StringCodec(),
                                              new StringCodec(),
                                              PeImportEntry::new),
                            ArrayList::new));

    /**
     * UUIDs of the imported symbols for PE.
     */
    public final static AuxDataSchema<List<UUID>> peImportedSymbols =
        new AuxDataSchema<>("peImportedSymbols",
                            new ListCodec<>(new UuidCodec(), ArrayList::new));

    /**
     * List of PE resources.
     */
    public final static AuxDataSchema<List<PeResourceEntry>> peResource =
        new AuxDataSchema<>(
            "peResource",
            new ListCodec<>(
                new Tuple3Codec<>(
                    new ListCodec<>(ByteCodec.UINT8, ArrayList::new),
                    new OffsetCodec(), LongCodec.UINT64, PeResourceEntry::new),
                ArrayList::new));

    /**
     * Profiling data. Executions by {@link CodeBlock}
     */
    public final static AuxDataSchema<Map<UUID, Long>> profile =
        new AuxDataSchema<>(
            "profile",
            new MapCodec<>(new UuidCodec(), LongCodec.UINT64, HashMap::new));

    /**
     * Map of function UUIDs to their associated typeTable entries for the
     * purpose of giving them prototypes.
     */
    public final static AuxDataSchema<Map<UUID, UUID>> prototypeTable =
        new AuxDataSchema<>(
            "prototypeTable",
            new MapCodec<>(new UuidCodec(), new UuidCodec(), HashMap::new));

    /**
     * The intra-procedural SCC identifier of each {@link CodeBlock}.
     */
    public final static AuxDataSchema<Map<UUID, Long>> sccs =
        new AuxDataSchema<>(
            "SCCs",
            new MapCodec<>(new UuidCodec(), LongCodec.INT64, HashMap::new));

    /**
     * Map from an Offset of a {@link SymbolicExpression} in a {@link
     * ByteInterval} to its extent, a size in bytes.
     */
    public final static AuxDataSchema<Map<Offset, Long>>
        symbolicExpressionSizes = new AuxDataSchema<>(
            "symbolicExpressionSizes",
            new MapCodec<>(new OffsetCodec(), LongCodec.UINT64, HashMap::new));

    /**
     * Structured type information about objects.
     */
    public final static AuxDataSchema<Map<UUID, TypeTableEntry>> typeTable =
        new AuxDataSchema<>(
            "typeTable",
            new MapCodec<>(
                new UuidCodec(),
                new Variant11Codec<>(
                    LongCodec.UINT64,
                    new Tuple1Codec<>(ByteCodec.UINT8,
                                      TypeTableEntry.BoolType::new),
                    new Tuple2Codec<>(ByteCodec.INT8, LongCodec.UINT64,
                                      TypeTableEntry.IntType::new),
                    LongCodec.UINT64, LongCodec.UINT64,
                    new Tuple2Codec<>(
                        new UuidCodec(),
                        new ListCodec<>(new UuidCodec(), ArrayList::new),
                        TypeTableEntry.FunctionType::new),
                    new UuidCodec(),
                    new Tuple2Codec<>(new UuidCodec(), LongCodec.UINT64,
                                      TypeTableEntry.ArrayType::new),
                    new UuidCodec(),
                    new Tuple2Codec<>(
                        LongCodec.UINT64,
                        new ListCodec<>(
                            new Tuple2Codec<>(LongCodec.UINT64, new UuidCodec(),
                                              TypeTableEntry.StructField::new),
                            ArrayList::new),
                        TypeTableEntry.StructType::new),
                    new Tuple1Codec<>(ByteCodec.UINT8,
                                      TypeTableEntry.VoidType::new),
                    TypeTableEntry::makeUnknown, TypeTableEntry::makeBool,
                    TypeTableEntry::makeInt, TypeTableEntry::makeChar,
                    TypeTableEntry::makeFloat, TypeTableEntry::makeFunction,
                    TypeTableEntry::makePointer, TypeTableEntry::makeArray,
                    TypeTableEntry::makeAlias, TypeTableEntry::makeStruct,
                    TypeTableEntry::makeVoid),
                HashMap::new));
}
