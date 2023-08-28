package com.grammatech.gtirb;

import com.grammatech.gtirb.tuple.Tuple4;

/**
 * A tuple representing an import entry for a PE file.
 */
public class PeImportEntry extends Tuple4<Long, Long, String, String> {
    /**
     * Constructor.
     *
     * @param address The address of the imported symbol.
     * @param ordinal The ordinal of the imported symbol.
     * @param funcName The name of the imported symbol.
     * @param libName The name of the library the symbol is imported from.
     */
    public PeImportEntry(Long address, Long ordinal, String funcName,
                         String libName) {
        super(address, ordinal, funcName, libName);
    }

    /**
     * Get the address.
     */
    public Long getAddress() { return this.get0(); }

    /**
     * Get the ordinal.
     */
    public Long getOrdinal() { return this.get1(); }

    /**
     * Get the function name.
     */
    public String getFuncName() { return this.get2(); }

    /**
     * Get the library name.
     */
    public String getLibName() { return this.get3(); }
}
