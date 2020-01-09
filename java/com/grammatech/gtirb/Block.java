/** */
package com.grammatech.gtirb;

public class Block extends Node {
    private long offset;
    private com.grammatech.gtirb.proto.ByteIntervalOuterClass.Block.ValueCase valueCase;
    private CodeBlock codeBlock;
    private DataBlock dataBlock;

    public Block(com.grammatech.gtirb.proto.ByteIntervalOuterClass.Block protoBlock) {

        this.valueCase = protoBlock.getValueCase();
        if (this.valueCase
                == com.grammatech.gtirb.proto.ByteIntervalOuterClass.Block.ValueCase.CODE) {
            this.codeBlock = new CodeBlock(protoBlock.getCode());
            this.dataBlock = null;
        } else if (this.valueCase
                == com.grammatech.gtirb.proto.ByteIntervalOuterClass.Block.ValueCase.DATA) {
            this.dataBlock = new DataBlock(protoBlock.getData());
            this.codeBlock = null;
        } else {
            throw new IllegalArgumentException("Block must be either a CodeBlock or a DataBlock.");
        }
    }

    public long getOffset() {
        return this.offset;
    }

    public CodeBlock getCodeBlock() {
        return this.codeBlock;
    }

    public DataBlock getDataBlock() {
        return this.dataBlock;
    }
}
