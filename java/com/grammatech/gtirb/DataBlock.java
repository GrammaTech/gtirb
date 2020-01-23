/** */
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
}
