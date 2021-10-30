/*
 *  Copyright (C) 2021 GrammaTech, Inc.
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

import com.grammatech.gtirb.proto.ByteIntervalOuterClass;
import com.grammatech.gtirb.proto.CodeBlockOuterClass;
import com.grammatech.gtirb.proto.DataBlockOuterClass;

/**
 * The ByteBlock class is a base class for code blocks and data blocks.
 */
public class ByteBlock extends Block implements TreeListItem {

    private long size;
    private long offset;
    private ByteInterval byteInterval;

    /**
     * Class constructor for a ByteBlock from a protobuf byte block.
     * @param  protoBlock     The byte block as serialized into a protocol
     * buffer.
     * @param  byteInterval   The ByteInterval that owns this ByteBlock.
     */
    public ByteBlock(ByteIntervalOuterClass.Block protoBlock,
                     ByteInterval byteInterval) {

        if (protoBlock.getValueCase() ==
            ByteIntervalOuterClass.Block.ValueCase.CODE) {
            CodeBlockOuterClass.CodeBlock codeBlock = protoBlock.getCode();
            this.size = codeBlock.getSize();
        } else if (protoBlock.getValueCase() ==
                   ByteIntervalOuterClass.Block.ValueCase.DATA) {
            DataBlockOuterClass.DataBlock dataBlock = protoBlock.getData();
            this.size = dataBlock.getSize();
        } else {
            throw new IllegalArgumentException(
                "Block must be either a CodeBlock or a DataBlock.");
        }
        this.offset = protoBlock.getOffset();
        this.byteInterval = byteInterval;
    }

    /**
     * Class Constructor.
     * @param  size           The number of bytes in this ByteBlock.
     * @param  offset         The offset of this Block in the ByteInterval.
     * @param  byteInterval   The ByteInterval that owns this ByteBlock.
     */
    public ByteBlock(long size, long offset, ByteInterval byteInterval) {
        this.size = size;
        this.offset = offset;
        this.byteInterval = byteInterval;
    }

    /**
     * Get the size of this ByteBlock.
     *
     * @return  The number of bytes in this ByteBlock.
     */
    public long getSize() { return this.size; }

    /**
     * Set the size of this ByteBlock.
     *
     * @param newSize    The new size to give to this ByteBlock.
     * @return           The new number of bytes in this ByteBlock.
     */
    public long setSize(long newSize) {
        this.size = newSize;
        return this.size;
    }

    /**
     * Get the offset of this ByteBlock.
     *
     * @return  The difference in address of the beginning of this ByteBlock's
     * ByteInterval and the beginning of this ByteBlock.
     */
    public long getOffset() { return this.offset; }

    /**
     * Get the index to manage this ByteBlock with.
     *
     * This is the index is used for storing and retrieving the ByteBlock, as
     * required by the TreeListItem interface. ByteBlocks are ordered by
     * offset, so this method just returns the offset.
     * @return  The ByteBlock index, which is it's offset.
     */
    public long getIndex() { return this.offset; }

    /**
     * Get the address of this ByteBlock.
     *
     * @return  The ByteBlock address, if the ByteInterval has an address,
     * otherwise null.
     */
    public long getAddress() {
        if (byteInterval == null || byteInterval.getAddress() == Util.NIL_ADDR)
            return Util.NIL_ADDR;
        else
            return byteInterval.getAddress() + this.offset;
    }

    /**
     * Get the ByteInterval that owns this ByteBlock.
     *
     * @return  The ByteInterval this ByteBlock belongs to. If it does not
     * belong to a ByteInterval, returns null.
     */
    public ByteInterval getByteInterval() { return this.byteInterval; }

    /**
     * De-serialize a ByteBlock from a protobuf .
     *
     * @param  protoByteBlock The byte block as serialized into a protocol
     * buffer.
     * @param  byteInterval   The ByteInterval that owns this ByteBlock.
     * @return An initialized ByteBlock.
     */
    public static ByteBlock
    fromProtobuf(ByteIntervalOuterClass.Block protoByteBlock,
                 ByteInterval byteInterval) {
        return new ByteBlock(protoByteBlock, byteInterval);
    }

    /**
     * Serialize this ByteBlock into a protobuf.
     *
     * This method is intended to be overridden on the base classes.
     *
     * @return Block protocol buffer containing this ByteBlock.
     */
    public ByteIntervalOuterClass.Block.Builder toProtobuf() {
        return ByteIntervalOuterClass.Block.newBuilder();
    }
}
