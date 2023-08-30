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

import java.io.*;
import java.util.List;
import java.util.function.Supplier;

public class ListCodec<T> implements Codec<List<T>> {
    private Codec<T> tCodec;
    private Supplier<List<T>> sup;

    public ListCodec(Codec<T> tc, Supplier<List<T>> s) {
        this.tCodec = tc;
        this.sup = s;
    }

    public String getTypeName() {
        return "sequence<" + this.tCodec.getTypeName() + ">";
    }

    public List<T> decode(InputStream in) throws IOException {
        List<T> al = this.sup.get();
        long len = LongCodec.decodeStatic(in);

        for (int i = 0; i < len; i++)
            al.add(this.tCodec.decode(in));
        return al;
    }

    public void encode(OutputStream out, List<T> al) throws IOException {
        LongCodec.encodeStatic(out, (long)al.size());
        for (T item : al) {
            this.tCodec.encode(out, item);
        }
    }
}
