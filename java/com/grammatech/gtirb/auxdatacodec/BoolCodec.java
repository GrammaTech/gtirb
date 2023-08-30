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

package com.grammatech.gtirb.auxdatacodec;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * A Codec for bool.
 */
public class BoolCodec implements Codec<Boolean> {

    public String getTypeName() { return "bool"; }

    public Boolean decode(InputStream in) throws IOException {
        byte[] b = new byte[1];
        if (in.read(b, 0, 1) != 1) {
            throw new EOFException(
                "Insufficient bytes to read a Boolean from.");
        }
        return Boolean.valueOf(b[0] != 0);
    }

    public void encode(OutputStream out, Boolean val) throws IOException {
        out.write((byte)(val ? 1 : 0));
    }
}
