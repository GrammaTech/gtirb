package com.grammatech.gtirb.auxdatacodec;

import com.grammatech.gtirb.variant.Variant3;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * A codec for serializing 3-element variants.
 */
public class Variant3Codec<T extends Variant3<A, B, C>, A, B, C>
    implements Codec<T> {
    private Codec<A> aCodec;
    private Codec<B> bCodec;
    private Codec<C> cCodec;

    private Variant3Maker<T, A> aMaker;
    private Variant3Maker<T, B> bMaker;
    private Variant3Maker<T, C> cMaker;

    public interface Variant3Maker<T, X> { public T make(X a); }

    /**
     * Construct a codec for 3-element variants.
     *
     * @param ac Codec for the first field of the variant.
     * @param bc Codec for the second field of the variant.
     * @param cc Codec for the third field of the variant.
     * @param aMaker Constructor/factory for constructing a variant
     * with the first field populated.
     * @param bMaker Constructor/factory for constructing a variant
     * with the second field populated.
     * @param cMaker Constructor/factory for constructing a variant
     * with the third field populated.
     */
    public Variant3Codec(Codec<A> ac, Codec<B> bc, Codec<C> cc,
                         Variant3Maker<T, A> aMaker, Variant3Maker<T, B> bMaker,
                         Variant3Maker<T, C> cMaker) {
        this.aCodec = ac;
        this.bCodec = bc;
        this.cCodec = cc;
        this.aMaker = aMaker;
        this.bMaker = bMaker;
        this.cMaker = cMaker;
    }

    public String getTypeName() {
        return "variant<" + aCodec.getTypeName() + "," + bCodec.getTypeName() +
            "," + cCodec.getTypeName() + ">";
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
        case 2: {
            C c = this.cCodec.decode(in);
            return this.cMaker.make(c);
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
        case 2: {
            this.cCodec.encode(out, val.get2().get());
            break;
        }
        default: {
            assert false;
        }
        }
    }
}
