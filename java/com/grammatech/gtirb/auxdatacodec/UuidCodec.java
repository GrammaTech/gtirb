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

package com.grammatech.gtirb.auxdatacodec;

import com.grammatech.gtirb.Util;
import java.io.*;
import java.util.UUID;

public class UuidCodec implements Codec<UUID> {

    public String getTypeName() { return "UUID"; }

    public static UUID decodeStatic(InputStream in) throws IOException {
        return Util.readUUID(in);
    }

    public UUID decode(InputStream in) throws IOException {
        return Util.readUUID(in);
    }

    public static void encodeStatic(OutputStream out, UUID val)
        throws IOException {
        Util.writeUUID(out, val);
    }

    public void encode(OutputStream out, UUID val) throws IOException {
        Util.writeUUID(out, val);
    }
}
