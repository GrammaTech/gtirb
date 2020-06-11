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

public class Block extends Node {
    private long offset;
    private com.grammatech.gtirb.proto.ByteIntervalOuterClass.Block
        .ValueCase valueCase;
    private CodeBlock codeBlock;
    private DataBlock dataBlock;
    private ByteInterval byteInterval;

    public Block(
        com.grammatech.gtirb.proto.ByteIntervalOuterClass.Block protoBlock,
        ByteInterval byteInterval) {

        this.offset = protoBlock.getOffset();
        this.valueCase = protoBlock.getValueCase();
        this.byteInterval = byteInterval;
        if (this.valueCase == com.grammatech.gtirb.proto.ByteIntervalOuterClass
                                  .Block.ValueCase.CODE) {
            this.codeBlock = new CodeBlock(protoBlock.getCode(), offset, this);
            this.dataBlock = null;
        } else if (this.valueCase ==
                   com.grammatech.gtirb.proto.ByteIntervalOuterClass.Block
                       .ValueCase.DATA) {
            this.dataBlock = new DataBlock(protoBlock.getData(), offset, this);
            this.codeBlock = null;
        } else {
            throw new IllegalArgumentException(
                "Block must be either a CodeBlock or a DataBlock.");
        }
    }

    public long getOffset() { return this.offset; }

    public CodeBlock getCodeBlock() { return this.codeBlock; }

    public DataBlock getDataBlock() { return this.dataBlock; }

    public ByteInterval getByteInterval() { return this.byteInterval; }
}
