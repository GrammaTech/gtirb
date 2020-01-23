/** */
package com.grammatech.gtirb;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.UUID;

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
