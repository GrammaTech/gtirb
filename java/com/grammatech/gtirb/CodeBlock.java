/** */
package com.grammatech.gtirb;

import java.util.UUID;

public class CodeBlock extends Node {
    private long size;
    private long decodeMode;

    public CodeBlock(com.grammatech.gtirb.proto.CodeBlockOuterClass.CodeBlock protoCodeBlock) {
        UUID uuid = Util.byteStringToUuid(protoCodeBlock.getUuid());
        super.setUuid(uuid);
        this.size = protoCodeBlock.getSize();
        this.decodeMode = protoCodeBlock.getDecodeMode();
    }

    public long getSize() {
        return size;
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
}
