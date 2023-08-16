/*
 *  Copyright (C) 2020-2023 GrammaTech, Inc.
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

import com.grammatech.gtirb.proto.SymbolicExpressionOuterClass;
import java.io.IOException;
import java.util.Set;
import java.util.UUID;

/**
 * The SymAddConst class represents a symbolic operand of the form "Sym +
 * Offset".
 */
public class SymAddrConst extends SymbolicExpression {

    private UUID symbol_uuid;
    private long offset;

    /**
     * Class constructor for a SymAddrConst from a protobuf symbolic expression.
     * @param  protoSymbolicExpression  The symbolic expression as serialized
     * into a protocol buffer.
     */
    public SymAddrConst(
        SymbolicExpressionOuterClass.SymbolicExpression protoSymbolicExpression)
        throws IOException {
        super(protoSymbolicExpression);
        SymbolicExpressionOuterClass.SymAddrConst protoSymAddrConst =
            protoSymbolicExpression.getAddrConst();
        this.setSymbolUuid(
            Util.byteStringToUuid(protoSymAddrConst.getSymbolUuid()));
        this.offset = protoSymAddrConst.getOffset();
    }

    /**
     * Class constructor for a SymAddrConst.
     * @param  offset          The offset of this symbolic expression in the
     * ByteInterval.
     * @param  symbol_uuid     The UUID of the symbolic operand.
     */
    public SymAddrConst(long offset, UUID symbol_uuid,
                        Set<AttributeFlag> attributeFlags) {
        super(attributeFlags);
        this.setSymbolUuid(symbol_uuid);
        this.offset = offset;
    }

    /**
     * Get the symbol UUID of this SymAddrConst.
     *
     * @return  The UUID of the symbolic operand of this expression.
     */
    public UUID getSymbolUuid() { return symbol_uuid; }

    /**
     * Set the symbol UUID of this SymAddrConst.
     *
     * @param  symbol_uuid    New value for the UUID of the symbolic operand of
     * the expression.
     */
    public void setSymbolUuid(UUID symbol_uuid) {
        this.symbol_uuid = symbol_uuid;
    }

    /**
     * Gets the symbol-relative constant offset of this SymAddrConst.
     * @return The current offset value
     */
    public long getOffset() { return this.offset; }

    /**
     * Sets the symbol-relative constant offset of this SymAddrConst.
     * @param offset New value for the constant offset.
     */
    public void setOffset(long offset) { this.offset = offset; }

    /**
     * De-serialize a {@link SymAddrConst} from a protobuf.
     *
     * @param  protoSymbolicExpression  The symbolic expression as serialized
     * into a protocol buffer.
     * @return An initialized SymAddrConst
     */
    public static SymAddrConst fromProtobuf(
        SymbolicExpressionOuterClass.SymbolicExpression protoSymbolicExpression)
        throws IOException {
        return new SymAddrConst(protoSymbolicExpression);
    }

    /**
     * Serialize this SymAddrConst into a protobuf.
     *
     * @return Block protocol buffer containing this SymAddrConst.
     */
    @Override
    public SymbolicExpressionOuterClass.SymbolicExpression.Builder
    toProtobuf() {
        SymbolicExpressionOuterClass.SymbolicExpression
            .Builder protoSymbolicExpression =
            SymbolicExpressionOuterClass.SymbolicExpression.newBuilder();
        SymbolicExpressionOuterClass.SymAddrConst.Builder protoSymAddrConst =
            SymbolicExpressionOuterClass.SymAddrConst.newBuilder();
        protoSymAddrConst.setSymbolUuid(
            Util.uuidToByteString(this.getSymbolUuid()));
        protoSymAddrConst.setOffset(this.getOffset());
        protoSymbolicExpression.setAddrConst(protoSymAddrConst);
        // NOTE for this to be valid, a one-to-one mapping of enums must be
        // maintained
        for (AttributeFlag attributeFlag : this.getAttributeFlags())
            protoSymbolicExpression.addAttributeFlagsValue(
                attributeFlag.ordinal());
        for (Integer value : this.getUnknownAttributeFlags()) {
            protoSymbolicExpression.addAttributeFlagsValue(value);
        }
        return protoSymbolicExpression;
    }
}
