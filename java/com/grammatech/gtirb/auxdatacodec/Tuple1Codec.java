package com.grammatech.gtirb.auxdatacodec;

import com.grammatech.gtirb.tuple.Tuple1;
import java.io.*;

public class Tuple1Codec<T extends Tuple1<A>, A> implements Codec<T> {
    private Codec<A> aCodec;

    private Tuple1Maker<T, A> maker;

    public interface Tuple1Maker<T, A> { public T make(A a); }

    public Tuple1Codec(Codec<A> ac, Tuple1Maker<T, A> maker) {
        this.aCodec = ac;
        this.maker = maker;
    }

    public String getTypeName() {
        return "tuple<" + aCodec.getTypeName() + ">";
    }

    public T decode(InputStream in) throws IOException {
        A a = this.aCodec.decode(in);
        return this.maker.make(a);
    }

    public void encode(OutputStream out, T val) throws IOException {
        this.aCodec.encode(out, val.get0());
    }
}
