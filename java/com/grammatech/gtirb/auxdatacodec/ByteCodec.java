package com.grammatech.gtirb.auxdatacodec;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;

public class ByteCodec implements Codec<Byte> {

    private String typeName;

    private ByteCodec(String tn) { this.typeName = tn; }

    public String getTypeName() { return this.typeName; }

    public Byte decode(InputStream in) throws IOException {
        byte[] b = new byte[1];
        if (in.read(b, 0, 1) != 1) {
            throw new EOFException("Insufficient bytes to read a Byte from.");
        }
        return b[0];
    }

    public void encode(OutputStream out, Byte val) throws IOException {
        byte[] b = new byte[1];
        b[0] = val;
        out.write(b, 0, 1);
    }

    public final static ByteCodec INT8 = new ByteCodec("int8_t");
    public final static ByteCodec UINT8 = new ByteCodec("uint8_t");
}
