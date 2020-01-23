package com.grammatech.gtirb;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public final class ByteInterval extends Node {

    private List<Block> blockList;
    private Long address;
    private long size;
    private ArrayList<Byte> bytes;

    public ByteInterval(com.grammatech.gtirb.proto.ByteIntervalOuterClass
                            .ByteInterval protoByteInterval) {
        UUID myUuid = Util.byteStringToUuid(protoByteInterval.getUuid());
        super.setUuid(myUuid);
        if (protoByteInterval.getHasAddress()) {
            this.address = Long.valueOf(protoByteInterval.getAddress());
        } else {
            this.address = null;
        }
        this.size = protoByteInterval.getSize();
        this.bytes = new ArrayList<Byte>();
        for (byte b : protoByteInterval.getContents().toByteArray()) {
            this.bytes.add(b);
        }

        List<com.grammatech.gtirb.proto.ByteIntervalOuterClass.Block>
            protoBlockList = protoByteInterval.getBlocksList();
        this.blockList = new ArrayList<Block>();
        for (com.grammatech.gtirb.proto.ByteIntervalOuterClass
                 .Block protoBlock : protoBlockList) {
            Block newBlock = new Block(protoBlock, myUuid);
            this.blockList.add(newBlock);
        }
    }

    public Long getAddress() { return this.address; }

    public boolean hasAddress() { return (this.getAddress() != null); }

    public List<Block> getBlockList() { return this.blockList; }

    public long getSize() { return this.size; }

    public ArrayList<Byte> getBytes() { return this.bytes; }
}
