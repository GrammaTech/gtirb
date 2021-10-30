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

package com.grammatech.gtirb;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.UUID;

/**
 * Serialization class wraps ByteBuffer for access to bytes.
 *
 */
public class Serialization {

    private ByteBuffer bb;
    private int remaining;

    /**
     * Constructor
     */
    public Serialization(byte[] bytes) {
        this.bb = ByteBuffer.wrap(bytes);
        this.bb.order(java.nio.ByteOrder.LITTLE_ENDIAN);
        this.remaining = bytes.length;
    }

    /**
     * Get the underlying ByteBuffer
     *
     * @return The byte buffer.
     */
    public ByteBuffer getByteBuffer() { return this.bb; }

    /**
     * Get a single byte from the byte buffer.
     *
     * @return The byte.
     */
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

    /**
     * Get a short int (2 bytes) from the byte buffer.
     *
     * @return The short.
     */
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

    /**
     * Get an int (4 bytes) from the byte buffer.
     *
     * @return The int.
     */
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

    /**
     * Get a long int (8 bytes) from the byte buffer.
     *
     * @return The long.
     */
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

    /**
     * Put a long int (8 bytes) into the byte buffer.
     *
     * @param value The long.
     */
    public ByteBuffer putLong(long value) {
        remaining -= 8;
        return (bb.putLong(value));
    }

    /**
     * Get a byte-swapped long from the byte buffer.
     *
     * @return The endian-changed long.
     */
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

    /**
     * Put a byte-swapped long into the byte buffer.
     *
     * @param value The long.
     */
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

    /**
     * Get a String from the byte buffer.
     *
     * @return The String.
     */
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

    /**
     * Put a String into the byte buffer.
     *
     * @param string The String.
     */
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

    /**
     * Get a UUID from the byte buffer.
     *
     * @return The UUID.
     */
    public UUID getUuid() {
        long longA = this.getByteSwappedLong();
        long longB = this.getByteSwappedLong();
        return new UUID(longA, longB);
    }

    /**
     * Put a UUID into the byte buffer.
     *
     * @param uuid The UUID.
     */
    public ByteBuffer putUuid(UUID uuid) {
        this.putByteSwappedLong(uuid.getMostSignificantBits());
        this.putByteSwappedLong(uuid.getLeastSignificantBits());
        return this.bb;
    }

    /**
     * Get remaining bytes
     *
     * @return How many bytes remain in byte buffer.
     */
    public int getRemaining() { return remaining; }
}
