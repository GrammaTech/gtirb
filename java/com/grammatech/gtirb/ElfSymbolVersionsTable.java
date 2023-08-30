package com.grammatech.gtirb;

import com.grammatech.gtirb.tuple.Tuple2;
import com.grammatech.gtirb.tuple.Tuple3;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * AuxData table for storing information about symbol versions in ELF files.
 */
public class ElfSymbolVersionsTable
    extends Tuple3<Map<Short, ElfSymbolVersionsTable.SymVerDef>,
                   Map<String, Map<Short, String>>,
                   Map<UUID, ElfSymbolVersionsTable.SymVerEntry>> {

    /**
     * A tuple for storing version strings and verdef flags.
     */
    public static class SymVerDef extends Tuple2<List<String>, Short> {
        /**
         * Constructor.
         *
         * @param versions The list of version strings for a version id.
         * @param verdef Verdef flag associated with a version id.
         */
        public SymVerDef(List<String> versions, Short verdef) {
            super(versions, verdef);
        }

        /**
         * Get the list of version strings.
         */
        public List<String> getVersions() { return this.get0(); }

        /**
         * Get the verdef flag.
         */
        public Short getVerdef() { return this.get1(); }
    }

    /**
     * A tuple for storing symbol version entries.
     */
    public static class SymVerEntry extends Tuple2<Short, Boolean> {
        /**
         * Constructor.
         *
         * @param verId The version id for the entry.
         * @param hidden Flag indicating if the symbol is hidden.
         */
        public SymVerEntry(Short verId, Boolean hidden) {
            super(verId, hidden);
        }

        /**
         * Get the version id.
         */
        public Short getVerId() { return this.get0(); }

        /**
         * True if the symbol is hidden
         */
        public Boolean isHidden() { return this.get1(); }
    }

    /**
     * Constructor.
     *
     * @param symVerDefMap A map of version ids to version definitions.
     * @param symVerNeededMap A map of dynamic library names to maps of symbol
     *     versions they need.
     * @param symVerEntriesMap A map of {@link Symbol} UUIDs to version ids.
     */
    public ElfSymbolVersionsTable(
        Map<Short, ElfSymbolVersionsTable.SymVerDef> symVerDefMap,
        Map<String, Map<Short, String>> symVerNeededMap,
        Map<UUID, ElfSymbolVersionsTable.SymVerEntry> symVerEntriesMap) {
        super(symVerDefMap, symVerNeededMap, symVerEntriesMap);
    }

    /**
     * Get the symbol version definition map.
     */
    public Map<Short, ElfSymbolVersionsTable.SymVerDef> getSymVerDefMap() {
        return this.get0();
    }

    /**
     * Get the map of versions needed by dynamic libraries.
     */
    public Map<String, Map<Short, String>> getVerNeededMap() {
        return this.get1();
    }

    /**
     * Get the symbol version entries map.
     */
    public Map<UUID, ElfSymbolVersionsTable.SymVerEntry> getSymVerEntriesMap() {
        return this.get2();
    }
}
