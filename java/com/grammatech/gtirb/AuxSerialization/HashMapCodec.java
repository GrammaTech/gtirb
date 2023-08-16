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

package com.grammatech.gtirb.AuxSerialization;

import com.grammatech.gtirb.AuxSerialization.LongCodec;
import java.io.*;
import java.util.HashMap;
import java.util.Map;

public class HashMapCodec<K, V> implements Codec<HashMap<K, V>> {
    private Codec<K> kCodec;
    private Codec<V> vCodec;

    public HashMapCodec(Codec<K> kc, Codec<V> vc) {
        this.kCodec = kc;
        this.vCodec = vc;
    }

    public String getTypeName() {
        return "map<" + kCodec.getTypeName() + "," + vCodec.getTypeName() + ">";
    }

    public HashMap<K, V> decode(InputStream in) throws IOException {
        HashMap<K, V> map = new HashMap<K, V>();

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

    public void encode(OutputStream out, HashMap<K, V> map) throws IOException {
        // Size of the map.
        LongCodec.encodeStatic(out, (long)map.size());

        // All the entries.
        for (Map.Entry<K, V> entry : map.entrySet()) {
            this.kCodec.encode(out, entry.getKey());
            this.vCodec.encode(out, entry.getValue());
        }
    }
}
