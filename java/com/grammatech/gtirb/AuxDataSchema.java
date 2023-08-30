package com.grammatech.gtirb;

import com.grammatech.gtirb.auxdatacodec.Codec;

public final class AuxDataSchema<T> {
    private String name;
    private Codec<T> codec;

    public AuxDataSchema(String n, Codec<T> c) {
        this.name = n;
        this.codec = c;
    }

    public String getName() { return this.name; }

    public Codec<T> getCodec() { return this.codec; }
}
