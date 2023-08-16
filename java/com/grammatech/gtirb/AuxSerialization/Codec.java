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

import java.io.*;

public interface Codec<T> {

    /**
     * Gets the portable name for the type used in protobuf.
     *
     * @return The name of the type.
     */
    public String getTypeName();

    /**
     * Decode a serialized instance into an in-memory object of type T.
     *
     * @param in The input stream the object is to be decoded from.
     * @return The decoded object.
     */
    public T decode(InputStream in) throws IOException;

    /**
     * Encode an in-memory object into serialized form.
     *
     * @param out The output stream to send the serialized object to.
     * @param val The in-memory object to be serialized.
     */
    public void encode(OutputStream out, T val) throws IOException;
}
