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

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public final class ByteInterval extends Node {

    private com.grammatech.gtirb.proto.ByteIntervalOuterClass
        .ByteInterval protoByteInterval;
    private List<Block> blockList;
    private Long address;
    private long size;
    private ArrayList<Byte> bytes;

    public ByteInterval(com.grammatech.gtirb.proto.ByteIntervalOuterClass
                            .ByteInterval protoByteInterval) {

        this.protoByteInterval = protoByteInterval;
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

    public byte[] getBytesDirect() {
        return protoByteInterval.getContents().toByteArray();
    }
}
