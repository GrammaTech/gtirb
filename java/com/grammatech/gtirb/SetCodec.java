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

package com.grammatech.gtirb;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.grammatech.gtirb.Serialization;
import com.grammatech.gtirb.TwoTuple;

/**
 * A Codec for set<T> entries. Implemented via HashSet.
 */
public class SetCodec extends Codec {

    public Object decode(Serialization byteBuffer,
                         List<TwoTuple<String, Object>> subtypes) {
        Set<Object> set = new HashSet<Object>();
        Long len = byteBuffer.getLong();
        TwoTuple<String, Object> subtype;
        if (subtypes.size() == 1)
            subtype = subtypes.get(0);
        else
            throw new DecodeException("could not unpack set type: " + subtypes);

        for (int i = 0; i < len; i++)
            set.add(AuxDataSerialization.decodeTree(byteBuffer, subtype));
        return set;
    }

    public void encode(StreamSerialization outstream, Object val,
                       List<TwoTuple<String, Object>> subtypes) {
        TwoTuple<String, Object> subtype;
        if (subtypes.size() == 1)
            subtype = subtypes.get(0);
        else
            throw new EncodeException("could not unpack set type: " + subtypes);
        if (val instanceof Set<?>) {
            Set<Object> set = (Set<Object>)val;
            outstream.putByteSwappedLong(set.size());
            for (Object item : set) {
                AuxDataSerialization.encodeTree(outstream, item, subtype);
            }
        } else
            throw new EncodeException("SetCodec: attempt to encode non set.");
    }
}
