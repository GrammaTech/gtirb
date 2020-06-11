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
import com.grammatech.gtirb.Block;

public class CodeBlock extends Node {
    private long offset;
    private long size;
    private long decodeMode;
    private Block block;

    public CodeBlock(
        com.grammatech.gtirb.proto.CodeBlockOuterClass.CodeBlock protoCodeBlock,
        long offset, Block block) {
        UUID myUuid = Util.byteStringToUuid(protoCodeBlock.getUuid());
        super.setUuid(myUuid);
        this.offset = offset;
        this.size = protoCodeBlock.getSize();
        this.decodeMode = protoCodeBlock.getDecodeMode();
        this.block = block;
    }

    public long getSize() { return size; }

    public long getOffset() { return offset; }

    public Block getBlock() { return this.block; }

    public void setSize(long size) { this.size = size; }

    public long getDecodeMode() { return decodeMode; }

    public void setDecodeMode(long decodeMode) { this.decodeMode = decodeMode; }
}
