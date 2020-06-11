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
import java.util.ArrayList;
import java.util.UUID;

import com.google.protobuf.ByteString;

public class Util {

    public static final UUID NIL_UUID = new UUID(0, 0);

    public static UUID
    byteStringToUuid(com.google.protobuf.ByteString byteString) {
        if (byteString == com.google.protobuf.ByteString.EMPTY) {
            return new UUID(0, 0);
        }
        byte[] uuidByteArray = byteString.toByteArray();
        ByteBuffer bb = ByteBuffer.wrap(uuidByteArray);
        return new UUID(bb.getLong(), bb.getLong());
    }

    public static com.google.protobuf.ByteString uuidToByteString(UUID uuid) {
        ByteBuffer bb = ByteBuffer.wrap(new byte[16]);
        bb.putLong(uuid.getMostSignificantBits());
        bb.putLong(uuid.getLeastSignificantBits());
        ByteString bs = ByteString.copyFrom(bb.array());
        return (bs);
    }

    public static byte[] toByteArray(ArrayList<Byte> in) {
        if (in == null) {
            return null;
        }
        final int n = in.size();
        byte ret[] = new byte[n];
        for (int i = 0; i < n; i++) {
            ret[i] = in.get(i);
        }
        return ret;
    }
}
