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

import java.util.ArrayList;
import java.util.List;

import com.grammatech.gtirb.Serialization;

/**
 * A Codec for tuple&lt;...&gt; entries. Implemented via ArrayList.
 */
public class TupleCodec extends Codec {

    public TupleCodec() {}

    public Object decode(Serialization byteBuffer, List<AuxTypeTree> subtypes) {
        List<Object> list = new ArrayList<Object>();

        for (AuxTypeTree subtype : subtypes) {
            Object item = AuxDataSerialization.decodeTree(byteBuffer, subtype);
            list.add(item);
        }
        return list;
    }

    public void encode(StreamSerialization outstream, Object val,
                       List<AuxTypeTree> subtypes) {
        // This allows TwoTuple, ThreeTuple, FiveTuple, and arbitrary-sized
        // object Lists
        if (!(val instanceof List<?>)) {
            throw new EncodeException(
                "TupleCodec: attempt to encode non tuple.");
        }

        List<?> tupleList = (List<?>)val;
        if (tupleList.size() != subtypes.size())
            throw new EncodeException(String.format(
                "TupleCodec: provided tuple data has %d items but schema expects %d.",
                tupleList.size(), subtypes.size()));
        for (int i = 0; i < tupleList.size(); i++)
            AuxDataSerialization.encodeTree(outstream, tupleList.get(i),
                                            subtypes.get(i));
    }
}
