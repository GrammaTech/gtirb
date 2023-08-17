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
import com.grammatech.gtirb.proto.DataBlockOuterClass;

/**
 * DataBlock represents a data object, possibly symbolic.
 */
public class DataBlock extends ByteBlock {

    /**
     * Class constructor for a {@link DataBlock} from a protobuf DataBlock.
     * @param  protoBlock  The DataBlock as serialized into a protocol buffer.
     */
    private DataBlock(ByteString protoUuid,
                      ByteIntervalOuterClass.Block protoBlock, long size) {
        super(protoUuid, protoBlock, size);
    }

    /**
     * Class constructor for a {@link DataBlock}.
     */
    public DataBlock(long size, long offset) { super(size, offset); }

    /**
     * De-serialize a {@link DataBlock} from a protobuf Block.
     *
     * @return An initialized DataBlock.
     */
    static DataBlock fromProtobuf(ByteIntervalOuterClass.Block protoBlock) {
        // Avoid using protoBlock.hasData() for compatibility with older
        // protobuf
        if (protoBlock.getValueCase() !=
            ByteIntervalOuterClass.Block.ValueCase.DATA) {
            return null;
        }
        DataBlockOuterClass.DataBlock protoDataBlock = protoBlock.getData();
        return new DataBlock(protoDataBlock.getUuid(), protoBlock,
                             protoDataBlock.getSize());
    }

    /**
     * Serialize this DataBlock into a protobuf.
     *
     * @return Block protocol buffer containing this DataBlock.
     */
    @Override
    ByteIntervalOuterClass.Block.Builder toProtobuf() {
        // The protoBlock is in ByteInterval outer class, and it gets a data
        // block added to it with the setData() method. So first create the
        // protoBlock, then create the protoDataBlock and add it to the
        // protoBlock.
        ByteIntervalOuterClass.Block.Builder protoBlock =
            ByteIntervalOuterClass.Block.newBuilder();

        DataBlockOuterClass.DataBlock.Builder protoDataBlock =
            DataBlockOuterClass.DataBlock.newBuilder();
        protoDataBlock.setUuid(Util.uuidToByteString(this.getUuid()));
        protoDataBlock.setSize(this.getSize());
        protoBlock.setOffset(this.getOffset());
        protoBlock.setData(protoDataBlock);
        return protoBlock;
    }
}
