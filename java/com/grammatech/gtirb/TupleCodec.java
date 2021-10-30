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

import java.util.ArrayList;
import java.util.List;

import com.grammatech.gtirb.Serialization;
import com.grammatech.gtirb.TwoTuple;

/**
 * A Codec for tuple<...> entries. Implemented via ArrayList.
 */
public class TupleCodec extends Codec {

    public TupleCodec() {}

    public Object decode(Serialization byteBuffer,
                         List<TwoTuple<String, Object>> subtypes) {
        List<Object> list = new ArrayList<Object>();

        for (TwoTuple<String, Object> subtype : subtypes) {
            Object item = AuxDataSerialization.decodeTree(byteBuffer, subtype);
            list.add(item);
        }
        return list;
    }

    public void encode(StreamSerialization outstream, Object val,
                       List<TwoTuple<String, Object>> subtypes) {
        if (val instanceof List<?>) {
            List<Object> tuple = (List<Object>)val;
            outstream.putByteSwappedLong(tuple.size());
            if (tuple.size() != subtypes.size())
                throw new EncodeException(
                    "TupleCodec: length of tuple does not match subtype count.");
            for (int i = 0; i < tuple.size(); i++)
                AuxDataSerialization.encodeTree(outstream, tuple.get(i),
                                                subtypes.get(i));
        } else
            throw new EncodeException(
                "TupleCodec: attempt to encode non tuple.");
    }
}
