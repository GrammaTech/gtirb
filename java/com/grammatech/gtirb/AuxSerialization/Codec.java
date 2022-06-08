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

// CONSIDER: Should this be an interface rather than an object hierarchy?
// What at all is gained from doing it this way?
// If there are classes without implementations of encode/decode, maybe _they_
// have a parent class.

import com.grammatech.gtirb.Serialization;
import java.util.List;

public class Codec {

    /**
     * Default constructor
     */
    public Codec() {}

    public Object decode(Serialization serialization,
                         List<AuxTypeTree> subtypes) {
        throw new Error("Codec.decode: Unknown Codec");
    }

    public void encode(StreamSerialization outstream, Object val,
                       List<AuxTypeTree> subtypes) {
        throw new Error("Codec.encode: Unknown Codec");
    }
}
