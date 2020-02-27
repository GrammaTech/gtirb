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

import java.util.UUID;

public class DataBlock extends Node {
    private long size;
    private long offset;
    private UUID byteIntervalUuid;

    public DataBlock(
        com.grammatech.gtirb.proto.DataBlockOuterClass.DataBlock protoDataBlock,
        long offset, UUID byteIntervalUuid) {
        UUID myUuid = Util.byteStringToUuid(protoDataBlock.getUuid());
        super.setUuid(myUuid);
        this.size = protoDataBlock.getSize();
        this.offset = offset;
        this.byteIntervalUuid = byteIntervalUuid;
    }

    public long getSize() { return this.size; }

    public long getOffset() { return this.offset; }

    public void setSize(long size) { this.size = size; }

    public UUID getByteIntervalUuid() { return this.byteIntervalUuid; }
}
