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

import java.io.*;
import java.nio.ByteBuffer;

public class FloatCodec implements Codec<Float> {

    public String getTypeName() { return "float"; }

    public Float decode(InputStream in) throws IOException {
        byte[] b = new byte[4];
        if (in.read(b, 0, 4) != 4) {
            throw new EOFException("Insufficient bytes to read a Float from.");
        }
        ByteBuffer bb = ByteBuffer.wrap(b);
        return bb.getFloat();
    }

    public void encode(OutputStream out, Float val) throws IOException {
        byte[] b = new byte[4];
        ByteBuffer bb = ByteBuffer.wrap(b);
        bb.putFloat(val);
        out.write(b, 0, 4);
    }
}
