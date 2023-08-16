package com.grammatech.gtirb;

import com.grammatech.gtirb.AuxSerialization.Codec;

public final class AuxDataSchema<T> {
    private String name;
    private Codec<T> codec;

    public AuxDataSchema(String n, Codec<T> c) {
        this.name = n;
        this.codec = c;
    }

    String getName() { return this.name; }

    Codec<T> getCodec() { return this.codec; }
}
