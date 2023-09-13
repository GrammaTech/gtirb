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

package com.grammatech.gtirb.auxdatacodec;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public class IntegerCodec implements Codec<Integer> {

    private String typeName;

    private IntegerCodec(String tn) { this.typeName = tn; }

    public String getTypeName() { return this.typeName; }

    public Integer decode(InputStream in) throws IOException {
        byte[] b = new byte[4];
        if (in.read(b, 0, 4) != 4) {
            throw new EOFException(
                "Insufficient bytes to read an Integer from.");
        }
        ByteBuffer bb = ByteBuffer.wrap(b);
        bb.order(ByteOrder.LITTLE_ENDIAN);
        return bb.getInt();
    }

    public void encode(OutputStream out, Integer val) throws IOException {
        byte[] b = new byte[4];
        ByteBuffer bb = ByteBuffer.wrap(b);
        bb.order(ByteOrder.LITTLE_ENDIAN);
        bb.putInt(val);
        out.write(b, 0, 4);
    }

    public final static IntegerCodec INT32 = new IntegerCodec("int32_t");
    public final static IntegerCodec UINT32 = new IntegerCodec("uint32_t");
}
