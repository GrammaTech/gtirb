package com.grammatech.gtirb;

import com.grammatech.gtirb.auxdatacodec.Codec;

/**
 * This class mediates encoding and decoding of individual AuxData tables.
 *
 * Generally, a single instance should be created for each type of table. That
 * instance is then used to fetch and store content from/to an {@link
 * AuxDataContainer}.
 */
public final class AuxDataSchema<T> {
    private String name;
    private Codec<T> codec;

    /**
     * Constructor.
     *
     * @param name The name of the AuxData table. Each schema should have a
     *     unique name.
     * @param codec The codec governing encoding and decoding.
     */
    public AuxDataSchema(String name, Codec<T> codec) {
        this.name = name;
        this.codec = codec;
    }

    /**
     * Get the name of the AuxData table.
     */
    public String getName() { return this.name; }

    /**
     * Get the codec to be used in encoding/decoding.
     */
    public Codec<T> getCodec() { return this.codec; }
}
