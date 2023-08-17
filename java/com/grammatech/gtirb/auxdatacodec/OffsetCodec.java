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

import com.grammatech.gtirb.Offset;
import java.io.*;
import java.util.UUID;

public class OffsetCodec implements Codec<Offset> {

    public String getTypeName() { return "Offset"; }

    public Offset decode(InputStream in) throws IOException {
        UUID uuid = UuidCodec.decodeStatic(in);
        long disp = LongCodec.decodeStatic(in);
        return new Offset(uuid, disp);
    }

    public void encode(OutputStream out, Offset val) throws IOException {
        UuidCodec.encodeStatic(out, val.getElementId());
        LongCodec.encodeStatic(out, val.getDisplacement());
    }
}
