package com.grammatech.gtirb;

import com.grammatech.gtirb.tuple.Tuple3;

/**
 * A tuple storing information about a PE file export entry.
 */
public class PeExportEntry extends Tuple3<Long, Long, String> {
    /**
     * Constructor.
     *
     * @param address The address of the export.
     * @param ordinal The ordinal of the export.
     * @param name The name of the export.
     */
    public PeExportEntry(Long address, Long ordinal, String name) {
        super(address, ordinal, name);
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
     * Get the name.
     */
    public String getName() { return this.get2(); }
}
