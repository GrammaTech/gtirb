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

import com.grammatech.gtirb.tuple.Tuple3;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class Tuple3Codec<T extends Tuple3<A, B, C>, A, B, C>
    implements Codec<T> {
    private Codec<A> aCodec;
    private Codec<B> bCodec;
    private Codec<C> cCodec;

    private Tuple3Maker<T, A, B, C> maker;

    public interface Tuple3Maker<T, A, B, C> { public T make(A a, B b, C c); }

    public Tuple3Codec(Codec<A> ac, Codec<B> bc, Codec<C> cc,
                       Tuple3Maker<T, A, B, C> maker) {
        this.aCodec = ac;
        this.bCodec = bc;
        this.cCodec = cc;
        this.maker = maker;
    }

    public String getTypeName() {
        return "tuple<" + aCodec.getTypeName() + "," + bCodec.getTypeName() +
            "," + cCodec.getTypeName() + ">";
    }

    public T decode(InputStream in) throws IOException {
        A a = this.aCodec.decode(in);
        B b = this.bCodec.decode(in);
        C c = this.cCodec.decode(in);
        return this.maker.make(a, b, c);
    }

    public void encode(OutputStream out, T val) throws IOException {
        this.aCodec.encode(out, val.get0());
        this.bCodec.encode(out, val.get1());
        this.cCodec.encode(out, val.get2());
    }
}
