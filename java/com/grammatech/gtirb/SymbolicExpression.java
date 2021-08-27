package com.grammatech.gtirb;

import java.util.ArrayList;
import java.util.List;
import com.grammatech.gtirb.proto.SymbolicExpressionOuterClass;

/**
 * The Symbolic Expression class is a base class for expressions such as
 * SymAddrConst, SymAddrAddr, and SymStackConst.
 */
public class SymbolicExpression implements TreeListItem {

    /**
     * 	Attributes for Symbolic Expressions
     */
    public enum AttributeFlag {
        Part0,
        Part1,
        Part2,
        Part3,
        Adjusted,
        GotRef,
        GotRelPC,
        GotRelGot,
        AddrRelGot,
        GotRelAddr,
        GotPage,
        GotPageOfst,
        PltRef
    }

    private long offset;
    private List<AttributeFlag> attributeFlags;

    // This needed no longer
    //    /**
    //     * Class constructor for a SymbolicExpression from a protobuf symbolic
    //     * expression.
    //     * @param  protoSymbolicExpression     The symbolic expression as
    //     serialized
    //     * into a protocol buffer.
    //     */
    //    public SymbolicExpression(
    //        SymbolicExpressionOuterClass.SymbolicExpression
    //        protoSymbolicExpression, long offset) { this.setOffset(offset);
    //        this.attributeFlags = new ArrayList<AttributeFlag>();
    //        for (Integer value :
    //        protoSymbolicExpression.getAttributeFlagsValueList()) {
    //            AttributeFlag attributeFlag = AttributeFlag.values()[value];
    //            this.attributeFlags.add(attributeFlag);
    //        }
    //    }

    // This is the constructor used when instantiating a sub class
    // NOTE: Offset is not set in constructor because subclasss doesn't
    // have it yet when calling super, so it sets it afterward.
    /**
     * Class constructor for a SymbolicExpression from a protobuf symbolic
     * expression.
     * @param  protoSymbolicExpression     The symbolic expression as serialized
     * into a protocol buffer.
     */
    public SymbolicExpression(SymbolicExpressionOuterClass
                                  .SymbolicExpression protoSymbolicExpression) {
        this.attributeFlags = new ArrayList<AttributeFlag>();
        for (Integer value :
             protoSymbolicExpression.getAttributeFlagsValueList()) {
            AttributeFlag attributeFlag = AttributeFlag.values()[value];
            this.attributeFlags.add(attributeFlag);
        }
    }

    /**
     * Class constructor for a SymbolicExpression.
     * @param  offset                      Address offset of this symbolic
     * expression in the ByteInterval.
     */
    public SymbolicExpression(long offset, List<AttributeFlag> attributeFlags) {
        this.setOffset(offset);
        this.setAttributeFlags(attributeFlags);
    }

    /**
     * Get the offset of this SymbolicExpression.
     *
     * @return  Difference in address from the start of the ByteInterval to the
     * start of the SymbolicExpression.
     */

    public long getOffset() { return offset; }

    /**
     * Get the index to manage this SymbolicExpression with.
     *
     * This is the index is used for storing and retrieving the
     * SymbolicExpression, as required by the TreeListItem interface.
     * SymbolicExpressions are ordered by offset, so this method just returns
     * the offset.
     * @return  The SymbolicExpression index, which is it's offset.
     */
    public long getIndex() { return this.offset; }

    /**
     * Get the size of this SymbolicExpression.
     *
     * @return  Always 0, because SymbolicExpressions by definition have no
     * size.
     */

    public long getSize() { return offset; }

    /**
     * Set the address of this SymbolicExpression.
     *
     * @param offset  New value for offset of this SymbolicExpression.
     */
    public void setOffset(long offset) { this.offset = offset; }

    /**
     * Get the flags applying to this SymbolicExpression.
     *
     * @return  A set of flags applying to this symbolic expression.
     */
    public List<AttributeFlag> getAttributeFlags() {
        return this.attributeFlags;
    }

    /**
     * Set the attribute flags of this SymbolicExpression.
     *
     * @param attributeFlags    A set of flags that will be applied to this
     * symbolic expression.
     */
    public void setAttributeFlags(List<AttributeFlag> attributeFlags) {
        this.attributeFlags = attributeFlags;
    }

    /**
     * De-serialize a {@link SymbolicExpression} from a protobuf .
     *
     * @param protoSymbolicExpression The protobuf version of this
     * symbolicExpression
     * @return An initialized SymbolicExpression.
     */
    public static SymbolicExpression
    fromProtobuf(SymbolicExpressionOuterClass
                     .SymbolicExpression protoSymbolicExpression) {
        return new SymbolicExpression(protoSymbolicExpression);
    }

    /**
     * Serialize this SymbolicExpression into a protobuf.
     *
     * This method is intended to be overwritten in the subclasses.
     *
     * @return Protocol buffer containing this Symbolic Expression.
     */
    public SymbolicExpressionOuterClass.SymbolicExpression.Builder
    toProtobuf() {
        return SymbolicExpressionOuterClass.SymbolicExpression.newBuilder();
    }
}
