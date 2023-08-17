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

import com.grammatech.gtirb.tuple.Pair;
import java.io.*;

public class PairCodec<T extends Pair<A, B>, A, B> implements Codec<T> {
    private Codec<A> aCodec;
    private Codec<B> bCodec;

    private PairMaker<T, A, B> maker;

    public interface PairMaker<T, A, B> { public T make(A a, B b); }

    public PairCodec(Codec<A> ac, Codec<B> bc, PairMaker<T, A, B> maker) {
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
        this.aCodec.encode(out, val.getFirst());
        this.bCodec.encode(out, val.getSecond());
    }
}
