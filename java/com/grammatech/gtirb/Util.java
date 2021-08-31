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
import java.nio.ByteBuffer;
import java.util.UUID;

/**
 * General purpose methods.
 */
public class Util {

    public static final UUID NIL_UUID = new UUID(0, 0);
    public static final long NIL_ADDR = 0;

    /**
     * Convert a ByteString to a UUID.
     *
     * @param byteString  The byte string.
     * @return            A UUID having the value retrieved from the byte
     * string.
     */
    public static UUID
    byteStringToUuid(com.google.protobuf.ByteString byteString) {
        if (byteString == com.google.protobuf.ByteString.EMPTY) {
            return new UUID(0, 0);
        }
        byte[] uuidByteArray = byteString.toByteArray();
        ByteBuffer bb = ByteBuffer.wrap(uuidByteArray);
        return new UUID(bb.getLong(), bb.getLong());
    }

    /**
     * Convert a UUID to a ByteString.
     *
     * @param uuid  The UUID.
     * @return      A ByteString storing the value of the UUID.
     */
    public static com.google.protobuf.ByteString uuidToByteString(UUID uuid) {
        ByteBuffer bb = ByteBuffer.wrap(new byte[16]);
        bb.putLong(uuid.getMostSignificantBits());
        bb.putLong(uuid.getLeastSignificantBits());
        ByteString bs = ByteString.copyFrom(bb.array());
        return (bs);
    }
}
