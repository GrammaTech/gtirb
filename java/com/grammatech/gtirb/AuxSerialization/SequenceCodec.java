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

import com.grammatech.gtirb.Serialization;

import java.util.ArrayList;
import java.util.List;

/**
 * A Codec for sequence<T> entries. Implemented via ArrayList.
 */
public class SequenceCodec extends Codec {

    /**
     * Default Constructor
     */
    public SequenceCodec() {}

    public Object decode(Serialization byteBuffer, List<AuxTypeTree> subtypes) {
        List<Object> sequence = new ArrayList<Object>();
        long len = byteBuffer.getLong();
        AuxTypeTree subtype;
        if (subtypes.size() == 1)
            subtype = subtypes.get(0);
        else
            throw new DecodeException("could not unpack sequence type: " +
                                      subtypes);

        for (int i = 0; i < len; i++)
            sequence.add(AuxDataSerialization.decodeTree(byteBuffer, subtype));
        return sequence;
    }

    public void encode(StreamSerialization outstream, Object val,
                       List<AuxTypeTree> subtypes) {
        AuxTypeTree subtype;
        if (subtypes.size() == 1)
            subtype = subtypes.get(0);
        else
            throw new DecodeException("could not unpack sequence type: " +
                                      subtypes);
        if (val instanceof List<?>) {
            List<Object> sequence = (List<Object>)val;
            outstream.putByteSwappedLong(sequence.size());
            for (Object item : sequence) {
                AuxDataSerialization.encodeTree(outstream, item, subtype);
            }
        } else
            throw new EncodeException(
                "SequenceCodec: attempt to encode non sequence.");
    }
}
