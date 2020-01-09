/** */
package com.grammatech.gtirb;

import java.nio.ByteBuffer;
import java.util.UUID;

public class Serialization {

    private ByteBuffer bb;
    private int remaining;

    public Serialization(byte[] bytes) {
        this.bb = ByteBuffer.wrap(bytes);
        this.bb.order(java.nio.ByteOrder.BIG_ENDIAN);
        this.remaining = bytes.length;
    }

    public int getSize() {
        // Call this at the beginning of a collection to get the
        // number of elements to expect. At that point, the next
        // byte will have the size, followed by 7 empty bytes.
        byte size = bb.get();
        // move position along a total of 8
        for (int i = 1; i < 8; i++) {
            bb.get();
        }
        remaining = remaining - 8;
        return (int) size;
    }

    public long getLong() {
        long retval;
        try {
            retval = bb.getLong();
        } catch (Exception e) {
            remaining = 0;
            return 0;
        }
        remaining = remaining - 8;
        return retval;
    }

    public UUID getUuid() {
        long longA = this.getLong();
        long longB = this.getLong();
        return new UUID(longA, longB);
    }

    public int getRemaining() {
        return remaining;
    }
}
