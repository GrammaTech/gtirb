/*
 *  Copyright (C) 2020 GrammaTech, Inc.
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

package com.grammatech.gtirb;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.UUID;

public class Serialization {

    private ByteBuffer bb;
    private int remaining;

    public Serialization(byte[] bytes) {
        this.bb = ByteBuffer.wrap(bytes);
        this.bb.order(java.nio.ByteOrder.LITTLE_ENDIAN);
        this.remaining = bytes.length;
    }

    public ByteBuffer getByteBuffer() { return this.bb; }

    private long getByteSwappedLong() {
        final int longSize = 8;
        byte[] swappedBytes = new byte[longSize];
        for (int i = 0; i < 8; i++) {
            swappedBytes[i] = bb.get();
        }
        remaining = remaining - longSize;
        ByteBuffer swappedBuffer = ByteBuffer.wrap(swappedBytes);
        return swappedBuffer.getLong();
    }

    private ByteBuffer putByteSwappedLong(long value) {
        final int longSize = 8;
        byte[] swappedBytes = new byte[longSize];
        ByteBuffer swappedBuffer = ByteBuffer.wrap(swappedBytes);
        swappedBuffer.putLong(value);
        for (int i = 0; i < 8; i++) {
            bb.put(swappedBytes[i]);
        }
        remaining = remaining - longSize;
        return this.bb;
    }

    public byte getByte() {
        byte retval;
        try {
            retval = bb.get();
        } catch (Exception e) {
            remaining = 0;
            return 0;
        }
        remaining = remaining - 1;
        return retval;
    }

    public short getShort() {
        short retval;
        try {
            retval = bb.getShort();
        } catch (Exception e) {
            remaining = 0;
            return 0;
        }
        remaining = remaining - 2;
        return retval;
    }

    public int getInt() {
        int retval;
        try {
            retval = bb.getInt();
        } catch (Exception e) {
            remaining = 0;
            return 0;
        }
        remaining = remaining - 4;
        return retval;
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

    public ByteBuffer putLong(long value) {
        remaining -= 8;
        return (bb.putLong(value));
    }

    public String getString() {
        int length = (int)getLong();
        byte[] strBytes = new byte[length];

        for (int i = 0; i < length; i++) {
            strBytes[i] = bb.get();
            remaining = remaining - 1;
        }
        String str = new String(strBytes, StandardCharsets.UTF_8);
        return str;
    }

    public ByteBuffer putString(String string) {
        byte[] strBytes = string.getBytes(StandardCharsets.UTF_8);
        long length = strBytes.length;
        bb.putLong(length);
        for (int i = 0; i < length; i++) {
            bb.put(strBytes[i]);
            remaining = remaining - 1;
        }
        return this.bb;
    }

    public UUID getUuid() {
        long longA = this.getByteSwappedLong();
        long longB = this.getByteSwappedLong();
        return new UUID(longA, longB);
    }

    public ByteBuffer putUuid(UUID uuid) {
        this.putByteSwappedLong(uuid.getMostSignificantBits());
        this.putByteSwappedLong(uuid.getLeastSignificantBits());
        return this.bb;
    }

    public int getRemaining() { return remaining; }
}
