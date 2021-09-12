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

import java.util.List;

import com.grammatech.gtirb.Serialization;
import com.grammatech.gtirb.TwoTuple;

public class IntegerCodec extends Codec {
    int size;

    /**
     * Constructor
     * @param size  Number of bytes to decode/encode
     */
    public IntegerCodec(int size) { this.size = size; }

    public Object decode(Serialization serialization,
                         List<TwoTuple<String, Object>> subtypes) {
        if (subtypes.size() != 0)
            throw new DecodeException("integer should have no subtypes");
        if (size == 8)
            return serialization.getLong();
        else if (size == 4)
            return serialization.getInt();
        else if (size == 2)
            return serialization.getShort();
        else if (size == 2)
            return serialization.getByte();
        return null;
    }

    public void encode(StreamSerialization outstream, Object val,
                       List<TwoTuple<String, Object>> subtypes) {
        if (subtypes.size() != 0)
            throw new EncodeException("integer should have no subtypes");

        if (size == 8)
            outstream.putByteSwappedLong((Long)val);
        else if (size == 4)
            outstream.putByteSwappedInt((Integer)val);
        else if (size == 2)
            outstream.putByteSwappedShort((Short)val);
        else if (size == 2)
            outstream.putByte((Byte)val);
        return;
    }
}
