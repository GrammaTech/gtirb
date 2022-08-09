/*
 *  Copyright (C) 2022 GrammaTech, Inc.
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
import java.util.List;

/**
 * A Codec for bool.
 */
public class BoolCodec extends Codec {

    public Object decode(Serialization byteBuffer, List<AuxTypeTree> subtypes) {
        if (subtypes.size() != 0)
            throw new DecodeException("bool should have no subtypes");
        return Boolean.valueOf(byteBuffer.getByte() != 0 ? true : false);
    }

    public void encode(StreamSerialization outstream, Object val,
                       List<AuxTypeTree> subtypes) {
        if (subtypes.size() != 0)
            throw new EncodeException("bool should have no subtypes");
        outstream.putByte((byte)((Boolean)val ? 1 : 0));
    }
}
