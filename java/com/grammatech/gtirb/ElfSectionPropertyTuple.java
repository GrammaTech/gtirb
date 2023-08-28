package com.grammatech.gtirb;

import com.grammatech.gtirb.tuple.Tuple2;

/**
 * A tuple for representing information about section properties in ELF files.
 */
public class ElfSectionPropertyTuple extends Tuple2<Long, Long> {
    /**
     * Constructor for building a tuple for a section.
     *
     * @param type The type of the section.
     * @param flags The flags for the section.
     */
    ElfSectionPropertyTuple(Long type, Long flags) { super(type, flags); }

    /**
     * Get the type of the section.
     *
     * @return The type of the section.
     */
    Long getType() { return this.get0(); }

    /**
     * Get the flags attached to the section.
     *
     * @return The flags for the section.
     */
    Long getFlags() { return this.get1(); }
}
