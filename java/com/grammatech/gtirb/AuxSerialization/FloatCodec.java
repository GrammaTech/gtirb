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

import java.util.List;

import com.grammatech.gtirb.Serialization;

public class FloatCodec extends Codec {
    int size;

    /**
     * Constructor
     * @param size  Number of bytes to decode/encode
     */
    public FloatCodec(int size) { this.size = size; }

    public Object decode(Serialization serialization,
                         List<AuxTypeTree> subtypes) {
        if (subtypes.size() != 0)
            throw new DecodeException("floats should have no subtypes");
        if (size == 8)
            return serialization.getDouble();
        else if (size == 4)
            return serialization.getFloat();
        throw new DecodeException("Invalid float size: " + size);
    }

    public void encode(StreamSerialization outstream, Object val,
                       List<AuxTypeTree> subtypes) {
        if (subtypes.size() != 0)
            throw new EncodeException("floats should have no subtypes");

        if (size == 8)
            outstream.putDouble((Double)val);
        else if (size == 4)
            outstream.putFloat((Float)val);
        else
            throw new EncodeException("invalid float size: " + size);
    }
}
