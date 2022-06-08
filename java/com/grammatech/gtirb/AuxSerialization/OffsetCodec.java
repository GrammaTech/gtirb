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

import com.grammatech.gtirb.Offset;
import com.grammatech.gtirb.Serialization;
import java.util.List;
import java.util.UUID;

public class OffsetCodec extends Codec {

    public Object decode(Serialization serialization,
                         List<AuxTypeTree> subtypes) {
        if (subtypes.size() != 0)
            throw new DecodeException("Offset should have no subtypes");
        UUID elementUuid = serialization.getUuid();
        long displacement = serialization.getLong();
        return new Offset(elementUuid, displacement);
    }

    public void encode(StreamSerialization outstream, Object val,
                       List<AuxTypeTree> subtypes) {
        if (subtypes.size() != 0)
            throw new EncodeException("Offset should have no subtypes");
        if (val instanceof Offset) {
            Offset offset = (Offset)val;
            outstream.putUuid(offset.getElementId());
            outstream.putByteSwappedLong(offset.getDisplacement());
        } else
            throw new EncodeException(
                "UuidCodec: attempt to encode non-UUID object");
    }
}
