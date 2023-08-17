/*
 *  Copyright (C) 2020-2021 GrammaTech, Inc.
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

import com.google.protobuf.ByteString;
import com.grammatech.gtirb.proto.ByteIntervalOuterClass;
import com.grammatech.gtirb.proto.CodeBlockOuterClass;

/**
 * CodeBlock represents a basic block in the binary.
 */
public class CodeBlock extends ByteBlock {
    /**
     * Variations on decoding a particular ISA
     */
    public enum DecodeMode {
        Default,
        Thumb,
    }

    private DecodeMode decodeMode;

    /**
     * Class constructor for a {@link CodeBlock} from a protobuf CodeBlock.
     * @param  protoBlock  The CodeBlock as serialized into a protocol buffer.
     */
    private CodeBlock(ByteString protoUuid,
                      ByteIntervalOuterClass.Block protoBlock, long size) {
        super(protoUuid, protoBlock, size);
        assert (protoBlock.getValueCase() ==
                ByteIntervalOuterClass.Block.ValueCase.CODE);
        CodeBlockOuterClass.CodeBlock protoCodeBlock = protoBlock.getCode();
        this.decodeMode =
            DecodeMode.values()[protoCodeBlock.getDecodeModeValue()];
    }

    /**
     * Class constructor for a {@link CodeBlock}.
     */
    public CodeBlock(long size, long offset, DecodeMode decodeMode) {
        super(size, offset);
        this.decodeMode = decodeMode;
    }

    /**
     * Get the decode mode of this {@link ByteBlock}.
     *
     * @return  The decode mode.
     */
    public DecodeMode getDecodeMode() { return decodeMode; }

    /**
     * Set the decode mode of this {@link CodeBlock}.
     *
     * @param decodeMode    The decode mode.
     */
    public void setDecodeMode(DecodeMode decodeMode) {
        this.decodeMode = decodeMode;
    }

    /**
     * De-serialize a {@link CodeBlock} from a protobuf Block.
     *
     * @return An initialized CodeBlock.
     */
    static CodeBlock fromProtobuf(ByteIntervalOuterClass.Block protoBlock) {
        // Avoid using protoBlock.hasCode() for compatibility with older
        // protobuf
        if (protoBlock.getValueCase() !=
            ByteIntervalOuterClass.Block.ValueCase.CODE) {
            return null;
        }
        CodeBlockOuterClass.CodeBlock protoCodeBlock = protoBlock.getCode();
        return new CodeBlock(protoCodeBlock.getUuid(), protoBlock,
                             protoCodeBlock.getSize());
    }

    /**
     * Serialize this {@link CodeBlock} into a protobuf.
     *
     * @return Block protocol buffer containing this CodeBlock.
     */
    @Override
    ByteIntervalOuterClass.Block.Builder toProtobuf() {
        // The protoBlock is in ByteInterval outer class, and it gets a code
        // block added to it with the setCode() method. So first create the
        // protoBlock, then create the protoCodeBlock and add it to the
        // protoBlock.
        ByteIntervalOuterClass.Block.Builder protoBlock =
            ByteIntervalOuterClass.Block.newBuilder();

        CodeBlockOuterClass.CodeBlock.Builder protoCodeBlock =
            CodeBlockOuterClass.CodeBlock.newBuilder();
        protoCodeBlock.setDecodeModeValue(this.decodeMode.ordinal());
        protoCodeBlock.setUuid(Util.uuidToByteString(this.getUuid()));
        protoCodeBlock.setSize(this.getSize());
        protoBlock.setOffset(this.getOffset());
        protoBlock.setCode(protoCodeBlock);
        return protoBlock;
    }
}
