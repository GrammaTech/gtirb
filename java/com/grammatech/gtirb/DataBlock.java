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
    public DataBlock(ByteIntervalOuterClass.Block protoBlock,
                     ByteInterval byteInterval) {
        // Could verify that this protoBlock is really Data.
        super(protoBlock, byteInterval);
        DataBlockOuterClass.DataBlock protoDataBlock = protoBlock.getData();
        this.uuid = Util.byteStringToUuid(protoDataBlock.getUuid());
    }

    /**
     * Class constructor for a {@link DataBlock}.
     */
    public DataBlock(long size, long offset, ByteInterval byteInterval) {
        super(size, offset, byteInterval);
        this.uuid = UUID.randomUUID();
    }

    /**
     * De-serialize a {@link DataBlock} from a protobuf .
     *
     * @return An initialized DataBlock.
     */
    public static DataBlock
    fromProtobuf(ByteIntervalOuterClass.Block protoBlock,
                 ByteInterval byteInterval) {
        return new DataBlock(protoBlock, byteInterval);
    }

    /**
     * Serialize this DataBlock into a protobuf.
     *
     * @param  protoBlock  The DataBlock as serialized into a protocol buffer.
     * @return Block protocol buffer containing this DataBlock.
     */
    @Override
    public ByteIntervalOuterClass.Block.Builder toProtobuf() {
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
