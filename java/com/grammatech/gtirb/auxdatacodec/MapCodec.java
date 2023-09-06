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
import java.util.Map;
import java.util.function.Supplier;

public class MapCodec<K, V> implements Codec<Map<K, V>> {
    private Codec<K> kCodec;
    private Codec<V> vCodec;
    private Supplier<Map<K, V>> sup;

    public MapCodec(Codec<K> kc, Codec<V> vc, Supplier<Map<K, V>> s) {
        this.kCodec = kc;
        this.vCodec = vc;
        this.sup = s;
    }

    public String getTypeName() {
        return "mapping<" + kCodec.getTypeName() + "," + vCodec.getTypeName() +
            ">";
    }

    public Map<K, V> decode(InputStream in) throws IOException {
        Map<K, V> map = this.sup.get();

        // Size of the map.
        long len = LongCodec.decodeStatic(in);

        // All the entries.
        for (int i = 0; i < len; i++) {
            K key = this.kCodec.decode(in);
            V val = this.vCodec.decode(in);
            map.put(key, val);
        }
        return map;
    }

    public void encode(OutputStream out, Map<K, V> map) throws IOException {
        // Size of the map.
        LongCodec.encodeStatic(out, (long)map.size());

        // All the entries.
        for (Map.Entry<K, V> entry : map.entrySet()) {
            this.kCodec.encode(out, entry.getKey());
            this.vCodec.encode(out, entry.getValue());
        }
    }
}
