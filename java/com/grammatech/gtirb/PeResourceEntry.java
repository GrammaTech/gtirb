package com.grammatech.gtirb;

import com.grammatech.gtirb.Offset;
import com.grammatech.gtirb.tuple.Tuple3;
import java.util.List;

/**
 * A tuple representing a resource attached to a PE file.
 */
public class PeResourceEntry extends Tuple3<List<Byte>, Offset, Long> {
    /**
     * Constructor.
     *
     * @param header The resource header.
     * @param offset Where the resource is located.
     * @param size The size of the resource data.
     */
    public PeResourceEntry(List<Byte> header, Offset offset, Long size) {
        super(header, offset, size);
    }

    /**
     * Get the header.
     */
    public List<Byte> getHeader() { return this.get0(); }

    /**
     * Get the offset.
     */
    public Offset getOffset() { return this.get1(); }

    /**
     * Get the size.
     */
    public Long getSize() { return this.get2(); }
}
