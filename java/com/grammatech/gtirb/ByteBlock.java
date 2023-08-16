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

import com.google.protobuf.ByteString;
import com.grammatech.gtirb.proto.ByteIntervalOuterClass;
import java.util.Optional;
import java.io.IOException;
import java.util.OptionalLong;

/**
 * The ByteBlock class is a base class for code blocks and data blocks.
 */
public abstract class ByteBlock extends Node implements TreeListItem {

    private long size;
    private long offset;
    private Optional<ByteInterval> byteInterval;

    /**
     * Class constructor for a ByteBlock from a protobuf byte block.
     * @param  protoBlock     The byte block as serialized into a protocol
     * buffer.
     * @param  size           The size of this ByteBlock in bytes.
     */
    ByteBlock(ByteString protoUuid, ByteIntervalOuterClass.Block protoBlock,
              long size) throws IOException {
        super(Util.byteStringToUuid(protoUuid));
        this.size = size;
        this.offset = protoBlock.getOffset();
        this.byteInterval = Optional.empty();
    }

    /**
     * Class Constructor.
     * @param  size           The number of bytes in this ByteBlock.
     * @param  offset         The offset of this Block in the ByteInterval.
     */
    public ByteBlock(long size, long offset) {
        super();
        this.size = size;
        this.offset = offset;
        this.byteInterval = Optional.empty();
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
    public OptionalLong getAddress() {
        if (this.byteInterval.isEmpty())
            return OptionalLong.empty();
        OptionalLong biAddress = byteInterval.get().getAddress();
        if (!biAddress.isPresent())
            return OptionalLong.empty();
        return OptionalLong.of(biAddress.getAsLong() + offset);
    }

    /**
     * Get the ByteInterval that owns this ByteBlock.
     *
     * @return  The ByteInterval this ByteBlock belongs to. If it does not
     * belong to a ByteInterval, returns null.
     */
    public Optional<ByteInterval> getByteInterval() {
        return this.byteInterval;
    }

    /**
     * Set the ByteInterval that owns this ByteBlock.
     *
     * @param  The ByteInterval this ByteBlock will belong to.
     */
    void setByteInterval(Optional<ByteInterval> byteInterval) {
        this.byteInterval = byteInterval;
    }

    /**
     * Serialize this ByteBlock into a protobuf.
     *
     * This method is intended to be overridden on the base classes.
     *
     * @return Block protocol buffer containing this ByteBlock.
     */
    ByteIntervalOuterClass.Block.Builder toProtobuf() {
        return ByteIntervalOuterClass.Block.newBuilder();
    }
}
