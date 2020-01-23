/** */
package com.grammatech.gtirb;

import java.util.UUID;

public class CodeBlock extends Node {
    private long offset;
    private long size;
    private long decodeMode;
    private UUID byteIntervalUuid;

    public CodeBlock(
            com.grammatech.gtirb.proto.CodeBlockOuterClass.CodeBlock protoCodeBlock,
            long offset,
            UUID byteIntervalUuid) {
        UUID myUuid = Util.byteStringToUuid(protoCodeBlock.getUuid());
        super.setUuid(myUuid);
        this.offset = offset;
        this.size = protoCodeBlock.getSize();
        this.decodeMode = protoCodeBlock.getDecodeMode();
        this.byteIntervalUuid = byteIntervalUuid;
    }

    public long getSize() {
        return size;
    }

    public long getOffset() {
        return offset;
    }

    public void setSize(long size) {
        this.size = size;
    }

    public long getDecodeMode() {
        return decodeMode;
    }

    public void setDecodeMode(long decodeMode) {
        this.decodeMode = decodeMode;
    }

    public UUID getByteIntervalUuid() {
        return this.byteIntervalUuid;
    }
}
