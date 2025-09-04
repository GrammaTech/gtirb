package com.grammatech.gtirb.auxdatacodec;

import com.grammatech.gtirb.variant.Variant11;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * A codec for serializing 3-element variants.
 */
public class Variant11Codec<
    T extends Variant11<A, B, C, D, E, F, G, H, I, J, K>, A, B, C, D, E, F, G,
              H, I, J, K> implements Codec<T> {
    private Codec<A> aCodec;
    private Codec<B> bCodec;
    private Codec<C> cCodec;
    private Codec<D> dCodec;
    private Codec<E> eCodec;
    private Codec<F> fCodec;
    private Codec<G> gCodec;
    private Codec<H> hCodec;
    private Codec<I> iCodec;
    private Codec<J> jCodec;
    private Codec<K> kCodec;

    private Variant11Maker<T, A> aMaker;
    private Variant11Maker<T, B> bMaker;
    private Variant11Maker<T, C> cMaker;
    private Variant11Maker<T, D> dMaker;
    private Variant11Maker<T, E> eMaker;
    private Variant11Maker<T, F> fMaker;
    private Variant11Maker<T, G> gMaker;
    private Variant11Maker<T, H> hMaker;
    private Variant11Maker<T, I> iMaker;
    private Variant11Maker<T, J> jMaker;
    private Variant11Maker<T, K> kMaker;

    public interface Variant11Maker<T, X> { public T make(X x); }

    /**
     * Construct a codec for 3-element variants.
     *
     * @param ac Codec for the first field of the variant.
     * @param bc Codec for the second field of the variant.
     * @param cc Codec for the third field of the variant.
     * @param dc Codec for the fourth field of the variant.
     * @param ec Codec for the fifth field of the variant.
     * @param fc Codec for the sixth field of the variant.
     * @param gc Codec for the seventh field of the variant.
     * @param hc Codec for the eighth field of the variant.
     * @param ic Codec for the ninth field of the variant.
     * @param jc Codec for the tenth field of the variant.
     * @param kc Codec for the eleventh field of the variant.
     * @param aMaker Constructor/factory for constructing a variant
     * with the first field populated.
     * @param bMaker Constructor/factory for constructing a variant
     * with the second field populated.
     * @param cMaker Constructor/factory for constructing a variant
     * with the third field populated.
     * @param dMaker Constructor/factory for constructing a variant
     * with the fourth field populated.
     * @param eMaker Constructor/factory for constructing a variant
     * with the fifth field populated.
     * @param fMaker Constructor/factory for constructing a variant
     * with the sixth field populated.
     * @param gMaker Constructor/factory for constructing a variant
     * with the seventh field populated.
     * @param hMaker Constructor/factory for constructing a variant
     * with the eighth field populated.
     * @param iMaker Constructor/factory for constructing a variant
     * with the ninth field populated.
     * @param jMaker Constructor/factory for constructing a variant
     * with the tenth field populated.
     * @param kMaker Constructor/factory for constructing a variant
     * with the eleventh field populated.
     */
    public Variant11Codec(
        Codec<A> ac, Codec<B> bc, Codec<C> cc, Codec<D> dc, Codec<E> ec,
        Codec<F> fc, Codec<G> gc, Codec<H> hc, Codec<I> ic, Codec<J> jc,
        Codec<K> kc, Variant11Maker<T, A> aMaker, Variant11Maker<T, B> bMaker,
        Variant11Maker<T, C> cMaker, Variant11Maker<T, D> dMaker,
        Variant11Maker<T, E> eMaker, Variant11Maker<T, F> fMaker,
        Variant11Maker<T, G> gMaker, Variant11Maker<T, H> hMaker,
        Variant11Maker<T, I> iMaker, Variant11Maker<T, J> jMaker,
        Variant11Maker<T, K> kMaker) {
        this.aCodec = ac;
        this.bCodec = bc;
        this.cCodec = cc;
        this.dCodec = dc;
        this.eCodec = ec;
        this.fCodec = fc;
        this.gCodec = gc;
        this.hCodec = hc;
        this.iCodec = ic;
        this.jCodec = jc;
        this.kCodec = kc;
        this.aMaker = aMaker;
        this.bMaker = bMaker;
        this.cMaker = cMaker;
        this.dMaker = dMaker;
        this.eMaker = eMaker;
        this.fMaker = fMaker;
        this.gMaker = gMaker;
        this.hMaker = hMaker;
        this.iMaker = iMaker;
        this.jMaker = jMaker;
        this.kMaker = kMaker;
    }

    public String getTypeName() {
        return "variant<" + aCodec.getTypeName() + "," + bCodec.getTypeName() +
            "," + cCodec.getTypeName() + "," + dCodec.getTypeName() + "," +
            eCodec.getTypeName() + "," + fCodec.getTypeName() + "," +
            gCodec.getTypeName() + "," + hCodec.getTypeName() + "," +
            iCodec.getTypeName() + "," + jCodec.getTypeName() + "," +
            kCodec.getTypeName() + ">";
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
        case 3: {
            D d = this.dCodec.decode(in);
            return this.dMaker.make(d);
        }
        case 4: {
            E e = this.eCodec.decode(in);
            return this.eMaker.make(e);
        }
        case 5: {
            F f = this.fCodec.decode(in);
            return this.fMaker.make(f);
        }
        case 6: {
            G g = this.gCodec.decode(in);
            return this.gMaker.make(g);
        }
        case 7: {
            H h = this.hCodec.decode(in);
            return this.hMaker.make(h);
        }
        case 8: {
            I i = this.iCodec.decode(in);
            return this.iMaker.make(i);
        }
        case 9: {
            J j = this.jCodec.decode(in);
            return this.jMaker.make(j);
        }
        case 10: {
            K k = this.kCodec.decode(in);
            return this.kMaker.make(k);
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
        case 3: {
            this.dCodec.encode(out, val.get3().get());
            break;
        }
        case 4: {
            this.eCodec.encode(out, val.get4().get());
            break;
        }
        case 5: {
            this.fCodec.encode(out, val.get5().get());
            break;
        }
        case 6: {
            this.gCodec.encode(out, val.get6().get());
            break;
        }
        case 7: {
            this.hCodec.encode(out, val.get7().get());
            break;
        }
        case 8: {
            this.iCodec.encode(out, val.get8().get());
            break;
        }
        case 9: {
            this.jCodec.encode(out, val.get9().get());
            break;
        }
        case 10: {
            this.kCodec.encode(out, val.get10().get());
            break;
        }
        default: {
            assert false;
        }
        }
    }
}
