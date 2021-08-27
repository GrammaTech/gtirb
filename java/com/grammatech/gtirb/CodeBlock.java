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
import com.grammatech.gtirb.ByteBlock;
import com.grammatech.gtirb.proto.ByteIntervalOuterClass;
import com.grammatech.gtirb.proto.CodeBlockOuterClass;

/**
 * CodeBlock represents a basic block in the binary.
 */
public class CodeBlock extends ByteBlock {
    private long decodeMode;

    /**
     * Class constructor for a {@link CodeBlock} from a protobuf CodeBlock.
     * @param  protoBlock  The CodeBlock as serialized into a protocol buffer.
     */
    public CodeBlock(ByteIntervalOuterClass.Block protoBlock,
                     ByteInterval byteInterval) {
        // Could verify that this protoBlock is really Code.
        super(protoBlock, byteInterval);
        CodeBlockOuterClass.CodeBlock protoCodeBlock = protoBlock.getCode();
        this.uuid = Util.byteStringToUuid(protoCodeBlock.getUuid());
        this.decodeMode = protoCodeBlock.getDecodeMode();
    }

    /**
     * Class constructor for a {@link CodeBlock}.
     */
    public CodeBlock(long size, long offset, long decodeMode,
                     ByteInterval byteInterval) {
        super(size, offset, byteInterval);
        this.uuid = UUID.randomUUID();
        this.decodeMode = decodeMode;
    }

    /**
     * Get the decode mode of this {@link ByteBlock}.
     *
     * @return  The decode mode.
     */
    public long getDecodeMode() { return decodeMode; }

    /**
     * Set the decode mode of this {@link CodeBlock}.
     *
     * @param decodeMode    The decode mode.
     */
    public void setDecodeMode(long decodeMode) { this.decodeMode = decodeMode; }

    /**
     * De-serialize a {@link CodeBlock} from a protobuf .
     *
     * @return An initialized CodeBlock.
     */
    public static CodeBlock
    fromProtobuf(ByteIntervalOuterClass.Block protoBlock,
                 ByteInterval byteInterval) {
        return new CodeBlock(protoBlock, byteInterval);
    }

    /**
     * Serialize this {@link CodeBlock} into a protobuf.
     *
     * @return Block protocol buffer containing this CodeBlock.
     */
    @Override
    public ByteIntervalOuterClass.Block.Builder toProtobuf() {
        // The protoBlock is in ByteInterval outer class, and it gets a code
        // block added to it with the setCode() method. So first create the
        // protoBlock, then create the protoCodeBlock and add it to the
        // protoBlock.
        ByteIntervalOuterClass.Block.Builder protoBlock =
            ByteIntervalOuterClass.Block.newBuilder();

        CodeBlockOuterClass.CodeBlock.Builder protoCodeBlock =
            CodeBlockOuterClass.CodeBlock.newBuilder();
        protoCodeBlock.setDecodeMode(this.getDecodeMode());
        protoCodeBlock.setUuid(Util.uuidToByteString(this.getUuid()));
        protoCodeBlock.setSize(this.getSize());
        protoBlock.setOffset(this.getOffset());
        protoBlock.setCode(protoCodeBlock);
        return protoBlock;
    }
}
