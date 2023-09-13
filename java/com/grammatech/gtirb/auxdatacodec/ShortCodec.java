package com.grammatech.gtirb.auxdatacodec;

import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public class ShortCodec implements Codec<Short> {

    private String typeName;

    private ShortCodec(String tn) { this.typeName = tn; }

    public String getTypeName() { return this.typeName; }

    public Short decode(InputStream in) throws IOException {
        byte[] b = new byte[2];
        if (in.read(b, 0, 2) != 2) {
            throw new EOFException("Insufficient bytes to read a Short from.");
        }
        ByteBuffer bb = ByteBuffer.wrap(b);
        bb.order(ByteOrder.LITTLE_ENDIAN);
        return bb.getShort();
    }

    public void encode(OutputStream out, Short val) throws IOException {
        byte[] b = new byte[2];
        ByteBuffer bb = ByteBuffer.wrap(b);
        bb.order(ByteOrder.LITTLE_ENDIAN);
        bb.putShort(val);
        out.write(b, 0, 2);
    }

    public final static ShortCodec INT16 = new ShortCodec("int16_t");
    public final static ShortCodec UINT16 = new ShortCodec("uint16_t");
}
