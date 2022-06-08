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

import com.grammatech.gtirb.proto.SymbolicExpressionOuterClass;
import java.util.List;
import java.util.UUID;

/**
 *
 * The SymAddrAddr class represents a symbolic operand of the form "(Sym1 -
 Sym2) / Scale + Offset".

 */
public class SymAddrAddr extends SymbolicExpression {

    private long scale;
    private UUID symbol1_uuid;
    private UUID symbol2_uuid;

    /**
     * Class constructor for a SymAddrAddr from a protobuf symbolic
     * expression.
     * @param  protoSymbolicExpression  The SymAddrAddr symbolic expression, as
     * serialized into a protocol buffer.
     */
    public SymAddrAddr(SymbolicExpressionOuterClass
                           .SymbolicExpression protoSymbolicExpression) {
        super(protoSymbolicExpression);
        SymbolicExpressionOuterClass.SymAddrAddr protoSymAddrAddr =
            protoSymbolicExpression.getAddrAddr();
        this.setSymbol1Uuid(
            Util.byteStringToUuid(protoSymAddrAddr.getSymbol1Uuid()));
        this.setSymbol2Uuid(
            Util.byteStringToUuid(protoSymAddrAddr.getSymbol2Uuid()));
        this.setScale(protoSymAddrAddr.getScale());
        super.setOffset(protoSymAddrAddr.getOffset());
    }

    /**
     * Class constructor for a SymbolicExpression.
     * @param  offset           The offset of this symbolic expression in the
     * ByteInterval.
     * @param  symbol1_uuid     The UUID of the first symbolic operand.
     * @param  symbol2_uuid     The UUID of the second symbolic operand.
     */
    public SymAddrAddr(long offset, long scale, UUID symbol1_uuid,
                       UUID symbol2_uuid, List<AttributeFlag> attributeFlags) {
        super(offset, attributeFlags);
        this.setScale(scale);
        this.setSymbol1Uuid(symbol1_uuid);
        this.setSymbol2Uuid(symbol2_uuid);
    }

    /**
     * Get the symbol UUID of the first operand in this SymAddrAddr.
     *
     * @return  The UUID of the first symbolic operand of this expression.
     */
    public UUID getSymbol1Uuid() { return symbol1_uuid; }

    /**
     * Set the symbol UUID of the first operand in this SymAddrAddr.
     *
     * @param symbol_uuid    New value for the UUID of the first symbolic
     * operand of the expression.
     */
    public void setSymbol1Uuid(UUID symbol_uuid) {
        this.symbol1_uuid = symbol_uuid;
    }

    /**
     * Get the symbol UUID of the second operand in this SymAddrAddr.
     *
     * @return  The UUID of the second symbolic operand of this expression.
     */
    public UUID getSymbol2Uuid() { return symbol2_uuid; }

    /**
     * Set the symbol UUID of the second operand in this SymAddrAddr.
     *
     * @param symbol_uuid    New value for the UUID of the second symbolic
     * operand of the expression.
     */
    public void setSymbol2Uuid(UUID symbol_uuid) {
        this.symbol2_uuid = symbol_uuid;
    }

    /**
     * Get the scale this SymAddrAddr.
     *
     * @return        The SymAddrAddr scale
     */
    public long getScale() { return scale; }

    /**
     * Set the scale this SymAddrAddr.
     *
     * @param scale    The new scale to set the SymAddrAddr scale to
     */
    public void setScale(long scale) { this.scale = scale; }

    /**
     * De-serialize a {@link SymAddrAddr} from a protobuf .
     *
     * @param  protoSymbolicExpression     The symbolic expression as serialized
     * into a protocol buffer.
     * @return An initialized SymAddrAddr.
     */
    public static SymAddrAddr
    fromProtobuf(SymbolicExpressionOuterClass
                     .SymbolicExpression protoSymbolicExpression) {
        return new SymAddrAddr(protoSymbolicExpression);
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
        SymbolicExpressionOuterClass.SymAddrAddr.Builder protoSymAddrAddr =
            SymbolicExpressionOuterClass.SymAddrAddr.newBuilder();
        protoSymAddrAddr.setSymbol1Uuid(
            Util.uuidToByteString(this.getSymbol1Uuid()));
        protoSymAddrAddr.setSymbol2Uuid(
            Util.uuidToByteString(this.getSymbol2Uuid()));
        protoSymAddrAddr.setOffset(this.getOffset());
        protoSymbolicExpression.setAddrAddr(protoSymAddrAddr);

        // NOTE for this to be valid, a one-to-one mapping of enums must be
        // maintained
        for (AttributeFlag attributeFlag : this.getAttributeFlags())
            protoSymbolicExpression.addAttributeFlagsValue(
                attributeFlag.ordinal());
        return protoSymbolicExpression;
    }
}
