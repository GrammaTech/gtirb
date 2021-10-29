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

/**
 * A Codec for strings.
 */
public class StringCodec extends Codec {

    public Object decode(Serialization byteBuffer, List<AuxTypeTree> subtypes) {
        if (subtypes.size() != 0)
            throw new DecodeException("string should have no subtypes");
        String string = byteBuffer.getString();
        return string;
    }

    public void encode(StreamSerialization outstream, Object val,
                       List<AuxTypeTree> subtypes) {
        if (subtypes.size() != 0)
            throw new EncodeException("string should have no subtypes");
        outstream.putString((String)val);
    }
}
