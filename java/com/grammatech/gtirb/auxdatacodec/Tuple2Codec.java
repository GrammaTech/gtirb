/*
 *  Copyright (C) 2020-2021 GrammaTech, Inc.
 *
 *  This code is licensed under the MIT license. See the LICENSE file in the
 *  project root for license terms.
 *
 *  This project is sponsored by the Office of Naval Research, One Liberty
 *  Center, 875 N. Randolph Street, Arlington, VA 22203 under contract #
 *  N68335-17-C-0700.  The content of the information does not necessarily
 *  reflect the position or policy of the Government and no official
 *  endorsement should be inferred.
 *
 */

package com.grammatech.gtirb.auxdatacodec;

import com.grammatech.gtirb.tuple.Tuple2;
import java.io.*;

public class Tuple2Codec<T extends Tuple2<A, B>, A, B> implements Codec<T> {
    private Codec<A> aCodec;
    private Codec<B> bCodec;

    private Tuple2Maker<T, A, B> maker;

    public interface Tuple2Maker<T, A, B> { public T make(A a, B b); }

    public Tuple2Codec(Codec<A> ac, Codec<B> bc, Tuple2Maker<T, A, B> maker) {
        this.aCodec = ac;
        this.bCodec = bc;
        this.maker = maker;
    }

    public String getTypeName() {
        return "tuple<" + aCodec.getTypeName() + "," + bCodec.getTypeName() +
            ">";
    }

    public T decode(InputStream in) throws IOException {
        A a = this.aCodec.decode(in);
        B b = this.bCodec.decode(in);
        return this.maker.make(a, b);
    }

    public void encode(OutputStream out, T val) throws IOException {
        this.aCodec.encode(out, val.get0());
        this.bCodec.encode(out, val.get1());
    }
}
