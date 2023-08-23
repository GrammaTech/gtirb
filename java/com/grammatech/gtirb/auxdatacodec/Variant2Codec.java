package com.grammatech.gtirb.auxdatacodec;

import com.grammatech.gtirb.variant.Variant2;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * A codec for serializing 2-element variants.
 */
public class Variant2Codec<T extends Variant2<A, B>, A, B> implements Codec<T> {
    private Codec<A> aCodec;
    private Codec<B> bCodec;

    private Variant2AMaker<T, A> aMaker;
    private Variant2BMaker<T, B> bMaker;

    public interface Variant2AMaker<T, A> { public T make(A a); }
    public interface Variant2BMaker<T, B> { public T make(B b); }

    /**
     * Construct a codec for 2-element variants.
     *
     * @param ac Codec for the first field of the variant.
     * @param bc Codec for the second field of the variant.
     * @param aMaker Constructor/factory for constructing a variant
     * with the first field populated.
     * @param bMaker Constructor/factory for constructing a variant
     * with the second field populated.
     */
    public Variant2Codec(Codec<A> ac, Codec<B> bc, Variant2AMaker<T, A> aMaker,
                         Variant2BMaker<T, B> bMaker) {
        this.aCodec = ac;
        this.bCodec = bc;
        this.aMaker = aMaker;
        this.bMaker = bMaker;
    }

    public String getTypeName() {
        return "variant<" + aCodec.getTypeName() + "," + bCodec.getTypeName() +
            ">";
    }

    public T decode(InputStream in) throws IOException {
        int idx = (int)LongCodec.decodeStatic(in);
        switch (idx) {
        case 0: {
            A a = this.aCodec.decode(in);
            return this.aMaker.make(a);
        }
        case 1: {
            B b = this.bCodec.decode(in);
            return this.bMaker.make(b);
        }
        }
        throw new IOException("Unexpected variant index: " + idx);
    }

    public void encode(OutputStream out, T val) throws IOException {
        int idx = val.getIndex();
        LongCodec.encodeStatic(out, (long)idx);
        switch (idx) {
        case 0: {
            this.aCodec.encode(out, val.get0().get());
            break;
        }
        case 1: {
            this.bCodec.encode(out, val.get1().get());
            break;
        }
        default: {
            assert false;
        }
        }
    }
}
