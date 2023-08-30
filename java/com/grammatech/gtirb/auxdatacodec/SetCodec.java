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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Set;
import java.util.function.Supplier;

public class SetCodec<T> implements Codec<Set<T>> {
    private Codec<T> tCodec;
    private Supplier<Set<T>> sup;

    public SetCodec(Codec<T> tc, Supplier<Set<T>> s) {
        this.tCodec = tc;
        this.sup = s;
    }

    public String getTypeName() {
        return "set<" + this.tCodec.getTypeName() + ">";
    }

    public Set<T> decode(InputStream in) throws IOException {
        Set<T> set = this.sup.get();

        // Size of the set.
        long len = LongCodec.decodeStatic(in);

        // All the entries.
        for (int i = 0; i < len; i++) {
            T item = this.tCodec.decode(in);
            set.add(item);
        }
        return set;
    }

    public void encode(OutputStream out, Set<T> set) throws IOException {
        // Size of the set.
        LongCodec.encodeStatic(out, (long)set.size());

        for (T item : set) {
            this.tCodec.encode(out, item);
        }
    }
}
