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

package com.grammatech.gtirb.AuxSerialization;

import java.io.*;
import java.nio.charset.StandardCharsets;

/**
 * A Codec for strings.
 */
public class StringCodec implements Codec<String> {

    public String getTypeName() { return "string"; }

    public String decode(InputStream in) throws IOException {
        int length = (int)LongCodec.decodeStatic(in);
        if (length == 0) {
            return "";
        }
        byte[] strBytes = new byte[length];
        if (in.read(strBytes, 0, length) != length) {
            throw new EOFException(
                "Insufficient bytes to read expected String length.");
        }

        return new String(strBytes, StandardCharsets.UTF_8);
    }

    public void encode(OutputStream out, String val) throws IOException {
        byte[] strBytes = val.getBytes(StandardCharsets.UTF_8);
        LongCodec.encodeStatic(out, (long)strBytes.length);
        out.write(strBytes, 0, strBytes.length);
    }
}
