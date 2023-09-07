package com.grammatech.gtirb;

import com.grammatech.gtirb.tuple.Tuple5;

/**
 * A tuple representing information about a {@link Symbol}.
 */
public class ElfSymbolInfoTuple
    extends Tuple5<Long, String, String, String, Long> {
    /**
     * Constructor.
     *
     * @param size The size of the {@link Symbol}.
     * @param type The type of the {@link Symbol}.
     * @param binding The binding type of the {@link Symbol}.
     * @param visibility What visibility level the {@link Symbol} has.
     * @param secIndex The index of the section the {@link Symbol} resides in.
     */
    public ElfSymbolInfoTuple(Long size, String type, String binding,
                              String visibility, Long secIndex) {
        super(size, type, binding, visibility, secIndex);
    }

    /**
     * Get the size.
     */
    public Long getSize() { return this.get0(); }

    /**
     * Get the type.
     */
    public String getType() { return this.get1(); }

    /**
     * Get the binding type.
     */
    public String getBinding() { return this.get2(); }

    /**
     * Get the visibility level.
     */
    public String getVisibility() { return this.get3(); }

    /**
     * Get the section index.
     */
    public Long getSecIndex() { return this.get4(); }
}
