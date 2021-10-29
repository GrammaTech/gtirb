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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.grammatech.gtirb.Serialization;

/**
 * A Codec for mapping&lt;K,V&gt; entries. Implemented via HashMap.
 */
public class MappingCodec extends Codec {

    public Object decode(Serialization byteBuffer, List<AuxTypeTree> subtypes)
        throws DecodeException {
        Map<Object, Object> mapping = new HashMap<Object, Object>();
        Long len = byteBuffer.getLong();
        AuxTypeTree keytype;
        AuxTypeTree valtype;
        if (subtypes.size() == 2) {
            keytype = subtypes.get(0);
            valtype = subtypes.get(1);
        } else
            throw new DecodeException("could not unpack mapping types: " +
                                      subtypes);

        for (int i = 0; i < len; i++) {
            Object key = AuxDataSerialization.decodeTree(byteBuffer, keytype);
            Object val = AuxDataSerialization.decodeTree(byteBuffer, valtype);
            mapping.put(key, val);
        }
        return mapping;
    }

    public void encode(StreamSerialization outstream, Object val,
                       List<AuxTypeTree> subtypes) {
        AuxTypeTree keytype;
        AuxTypeTree valtype;
        if (subtypes.size() == 2) {
            keytype = subtypes.get(0);
            valtype = subtypes.get(1);
        } else
            throw new EncodeException("could not unpack sequence type: " +
                                      subtypes);
        if (val instanceof Map<?, ?>) {
            Map<?, ?> map = (Map<?, ?>)val;
            Set<?> keys = map.keySet();
            outstream.putByteSwappedLong(map.size());
            for (Object key : keys) {
                AuxDataSerialization.encodeTree(outstream, key, keytype);
                AuxDataSerialization.encodeTree(outstream, map.get(key),
                                                valtype);
            }
        } else
            throw new EncodeException(
                "SequenceCodec: attempt to encode non sequence.");
    }
}
