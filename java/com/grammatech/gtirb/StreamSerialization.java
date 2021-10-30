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

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.UUID;

public class StreamSerialization {

    private DataOutputStream outstream;

    /**
     * Constructor
     */
    public StreamSerialization(DataOutputStream outstream) {
        this.outstream = outstream;
    }

    /**
     * Constructor
     */
    public StreamSerialization(ByteArrayOutputStream byteStream) {
        this.outstream = new DataOutputStream(byteStream);
    }

    /**
     * Put a byte into the output stream.
     *
     * @param value The byte.
     */
    public void putByte(byte value) {
        try {
            outstream.writeByte(value);
        } catch (IOException ex) {
            throw new EncodeException("Error writing to aux data stream.");
        }
        return;
    }

    /**
     * Put a short int (2 bytes) into the output stream.
     *
     * @param value The short.
     */
    public void putShort(short value) {
        try {
            outstream.writeShort(value);
        } catch (IOException ex) {
            throw new EncodeException("Error writing to aux data stream.");
        }
        return;
    }

    /**
     * Put a byte-swappedshort int (2 bytes) into the output stream.
     *
     * @param value The short.
     */
    public void putByteSwappedShort(short value) {
        try {
            final int size = 2;
            byte[] swappedBytes = new byte[size];
            ByteBuffer swappedBuffer = ByteBuffer.wrap(swappedBytes);
            swappedBuffer.putShort(value);
            for (int i = size - 1; i >= 0; i--) {
                outstream.writeByte(swappedBytes[i]);
            }
        } catch (IOException ex) {
            throw new EncodeException("Error writing to aux data stream.");
        }
        return;
    }

    /**
     * Put an int (4 bytes) into the output stream.
     *
     * @param value The int.
     */
    public void putInt(long value) {
        try {
            outstream.writeLong(value);
        } catch (IOException ex) {
            throw new EncodeException("Error writing to aux data stream.");
        }
        return;
    }

    /**
     * Put a byte-swapped int (4 bytes) into the output stream.
     *
     * @param value The int.
     */
    public void putByteSwappedInt(int value) {
        try {
            final int size = 4;
            byte[] swappedBytes = new byte[size];
            ByteBuffer swappedBuffer = ByteBuffer.wrap(swappedBytes);
            swappedBuffer.putInt(value);
            for (int i = size - 1; i >= 0; i--) {
                outstream.writeByte(swappedBytes[i]);
            }
        } catch (IOException ex) {
            throw new EncodeException("Error writing to aux data stream.");
        }
        return;
    }

    /**
     * Put a long int (8 bytes) into the output stream.
     *
     * @param value The long.
     */
    public void putLong(long value) {
        try {
            outstream.writeLong(value);
        } catch (IOException ex) {
            throw new EncodeException("Error writing to aux data stream.");
        }
        return;
    }

    /**
     * Put a byte-swapped long int (8 bytes) into the output stream.
     *
     * @param value The long.
     */
    public void putByteSwappedLong(long value) {
        try {
            final int longSize = 8;
            byte[] swappedBytes = new byte[longSize];
            ByteBuffer swappedBuffer = ByteBuffer.wrap(swappedBytes);
            swappedBuffer.putLong(value);
            for (int i = 7; i >= 0; i--) {
                outstream.writeByte(swappedBytes[i]);
            }
        } catch (IOException ex) {
            throw new EncodeException("Error writing long to stream.");
        }
        return;
    }

    /**
     * Put a String into the output stream.
     *
     * @param value The string.
     */
    public void putString(String value) {
        byte[] strBytes = value.getBytes(StandardCharsets.UTF_8);
        long length = strBytes.length;
        putByteSwappedLong(length);
        try {
            // outstream.writeLong(length);
            outstream.writeBytes(value);
        } catch (IOException ex) {
            throw new EncodeException("Error writing to aux data stream.");
        }
        return;
    }

    /**
     * Put a UUID into the output stream.
     *
     * @param uuid The UUID.
     */
    public void putUuid(UUID uuid) {
        //        this.putByteSwappedLong(uuid.getMostSignificantBits());
        //        this.putByteSwappedLong(uuid.getLeastSignificantBits());
        this.putLong(uuid.getMostSignificantBits());
        this.putLong(uuid.getLeastSignificantBits());
        return;
    }
}
