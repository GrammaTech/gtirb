/** */
package com.grammatech.gtirb;

import java.util.UUID;

public class DataBlock extends Node {
    private long size;

    public DataBlock(com.grammatech.gtirb.proto.DataBlockOuterClass.DataBlock protoDataBlock) {
        UUID uuid = Util.byteStringToUuid(protoDataBlock.getUuid());
        super.setUuid(uuid);
        this.size = protoDataBlock.getSize();
    }

    public long getSize() {
        return size;
    }

    public void setSize(long size) {
        this.size = size;
    }
}
