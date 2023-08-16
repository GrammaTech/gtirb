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

import com.google.protobuf.ByteString;
import java.io.*;
import java.lang.*;
import java.nio.ByteBuffer;
import java.util.UUID;

/**
 * General purpose methods.
 */
public class Util {

    public static final UUID NIL_UUID = new UUID(0, 0);
    public static final long NIL_ADDR = 0;

    /**
     * Converts a 16-byte array into a UUID.
     *
     * @param in The byte array.
     * @return The resulting UUID.
     */
    public static UUID byteArrayToUUID(byte[] b)
        throws IllegalArgumentException {
        if (b.length != 16) {
            throw new IllegalArgumentException("b.length() != 16");
        }
        ByteBuffer bb = ByteBuffer.wrap(b);
        return new UUID(bb.getLong(), bb.getLong());
    }

    /**
     * Reads a UUID from an InputStream.
     *
     * @param in The stream to read from.
     * @return The UUID read.
     */
    public static UUID readUUID(InputStream in) throws IOException {
        byte[] b = new byte[16];
        if (in.read(b, 0, 16) != 16) {
            throw new EOFException("Insufficient bytes to read a UUID from.");
        }
        try {
            return byteArrayToUUID(b);
        } catch (IllegalArgumentException e) {
            // Should never happen.
            throw new RuntimeException(
                "byteArrayToUUID() and readUUID using inconsistent buffer sizes!");
        }
    }

    /**
     * Convert a ByteString to a UUID.
     *
     * @param byteString  The byte string.
     * @return            A UUID having the value retrieved from the byte
     * string.
     */
    public static UUID
    byteStringToUuid(com.google.protobuf.ByteString byteString)
        throws IOException {
        if (byteString == com.google.protobuf.ByteString.EMPTY) {
            return new UUID(0, 0);
        }
        byte[] uuidByteArray = byteString.toByteArray();
        try {
            return byteArrayToUUID(uuidByteArray);
        } catch (IllegalArgumentException e) {
            throw new IOException(
                "Protobuf ByteString has insufficient length for a UUID!");
        }
    }

    /**
     * Converts a UUID to a 16-byte array
     *
     * @param uuid The uuid to convert.
     * @return The resulting 16-byte array.
     */
    public static byte[] uuidToByteArray(UUID uuid) {
        byte[] ba = new byte[16];
        ByteBuffer bb = ByteBuffer.wrap(ba);
        bb.putLong(uuid.getMostSignificantBits());
        bb.putLong(uuid.getLeastSignificantBits());
        return ba;
    }

    /**
     * Writes a UUID to an OutputStream
     *
     * @param out The stream to write to.
     */
    public static void writeUUID(OutputStream out, UUID uuid)
        throws IOException {
        byte[] b = uuidToByteArray(uuid);
        out.write(b, 0, 16);
    }

    /**
     * Convert a UUID to a ByteString.
     *
     * @param uuid  The UUID.
     * @return      A ByteString storing the value of the UUID.
     */
    public static com.google.protobuf.ByteString uuidToByteString(UUID uuid) {
        return ByteString.copyFrom(uuidToByteArray(uuid));
    }
}
