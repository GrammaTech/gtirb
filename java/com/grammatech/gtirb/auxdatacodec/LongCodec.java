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

public class LongCodec implements Codec<Long> {

    private String typeName;

    private LongCodec(String tn) { this.typeName = tn; }

    public String getTypeName() { return this.typeName; }

    public static long decodeStatic(InputStream in) throws IOException {
        byte[] b = new byte[8];
        if (in.read(b, 0, 8) != 8) {
            throw new EOFException("Insufficient bytes to read a Long from.");
        }
        ByteBuffer bb = ByteBuffer.wrap(b);
        bb.order(ByteOrder.LITTLE_ENDIAN);
        return bb.getLong();
    }

    public Long decode(InputStream in) throws IOException {
        return new Long(decodeStatic(in));
    }

    public static void encodeStatic(OutputStream out, long val)
        throws IOException {
        byte[] b = new byte[8];
        ByteBuffer bb = ByteBuffer.wrap(b);
        bb.order(ByteOrder.LITTLE_ENDIAN);
        bb.putLong(val);
        out.write(b, 0, 8);
    }

    public void encode(OutputStream out, Long val) throws IOException {
        encodeStatic(out, val);
    }

    public final static LongCodec INT64 = new LongCodec("int64_t");
    public final static LongCodec UINT64 = new LongCodec("uint64_t");
}
