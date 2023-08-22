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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * The Symbolic Expression class is a base class for expressions such as
 * SymAddrConst, SymAddrAddr, and SymStackConst.
 */
public class SymbolicExpression {

    /**
     * Attributes for Symbolic Expressions.
     *
     * @see https://grammatech.github.io/gtirb/md__symbolic_expression.html
     */
    public enum AttributeFlag {
        GOT(SymbolicExpressionOuterClass.SymAttribute.GOT_VALUE),
        GOTPC(SymbolicExpressionOuterClass.SymAttribute.GOTPC_VALUE),
        GOTOFF(SymbolicExpressionOuterClass.SymAttribute.GOTOFF_VALUE),
        GOTREL(SymbolicExpressionOuterClass.SymAttribute.GOTREL_VALUE),
        PLT(SymbolicExpressionOuterClass.SymAttribute.PLT_VALUE),
        PLTOFF(SymbolicExpressionOuterClass.SymAttribute.PLTOFF_VALUE),
        PCREL(SymbolicExpressionOuterClass.SymAttribute.PCREL_VALUE),
        SECREL(SymbolicExpressionOuterClass.SymAttribute.SECREL_VALUE),
        TLS(SymbolicExpressionOuterClass.SymAttribute.TLS_VALUE),
        TLSGD(SymbolicExpressionOuterClass.SymAttribute.TLSGD_VALUE),
        TLSLD(SymbolicExpressionOuterClass.SymAttribute.TLSLD_VALUE),
        TLSLDM(SymbolicExpressionOuterClass.SymAttribute.TLSLDM_VALUE),
        TLSCALL(SymbolicExpressionOuterClass.SymAttribute.TLSCALL_VALUE),
        TLSDESC(SymbolicExpressionOuterClass.SymAttribute.TLSDESC_VALUE),
        TPREL(SymbolicExpressionOuterClass.SymAttribute.TPREL_VALUE),
        TPOFF(SymbolicExpressionOuterClass.SymAttribute.TPOFF_VALUE),
        DTPREL(SymbolicExpressionOuterClass.SymAttribute.DTPREL_VALUE),
        DTPOFF(SymbolicExpressionOuterClass.SymAttribute.DTPOFF_VALUE),
        DTPMOD(SymbolicExpressionOuterClass.SymAttribute.DTPMOD_VALUE),
        NTPOFF(SymbolicExpressionOuterClass.SymAttribute.NTPOFF_VALUE),
        PAGE(SymbolicExpressionOuterClass.SymAttribute.PAGE_VALUE),
        PAGEOFF(SymbolicExpressionOuterClass.SymAttribute.PAGEOFF_VALUE),
        CALL(SymbolicExpressionOuterClass.SymAttribute.CALL_VALUE),
        LO(SymbolicExpressionOuterClass.SymAttribute.LO_VALUE),
        HI(SymbolicExpressionOuterClass.SymAttribute.HI_VALUE),
        HIGHER(SymbolicExpressionOuterClass.SymAttribute.HIGHER_VALUE),
        HIGHEST(SymbolicExpressionOuterClass.SymAttribute.HIGHEST_VALUE),
        GOTNTPOFF(SymbolicExpressionOuterClass.SymAttribute.GOTNTPOFF_VALUE),
        INDNTPOFF(SymbolicExpressionOuterClass.SymAttribute.INDNTPOFF_VALUE),
        G0(SymbolicExpressionOuterClass.SymAttribute.G0_VALUE),
        G1(SymbolicExpressionOuterClass.SymAttribute.G1_VALUE),
        G2(SymbolicExpressionOuterClass.SymAttribute.G2_VALUE),
        G3(SymbolicExpressionOuterClass.SymAttribute.G3_VALUE),
        UPPER16(SymbolicExpressionOuterClass.SymAttribute.UPPER16_VALUE),
        LOWER16(SymbolicExpressionOuterClass.SymAttribute.LOWER16_VALUE),
        LO12(SymbolicExpressionOuterClass.SymAttribute.LO12_VALUE),
        LO15(SymbolicExpressionOuterClass.SymAttribute.LO15_VALUE),
        LO14(SymbolicExpressionOuterClass.SymAttribute.LO14_VALUE),
        HI12(SymbolicExpressionOuterClass.SymAttribute.HI12_VALUE),
        HI21(SymbolicExpressionOuterClass.SymAttribute.HI21_VALUE),
        S(SymbolicExpressionOuterClass.SymAttribute.S_VALUE),
        PG(SymbolicExpressionOuterClass.SymAttribute.PG_VALUE),
        NC(SymbolicExpressionOuterClass.SymAttribute.NC_VALUE),
        ABS(SymbolicExpressionOuterClass.SymAttribute.ABS_VALUE),
        PREL(SymbolicExpressionOuterClass.SymAttribute.PREL_VALUE),
        PREL31(SymbolicExpressionOuterClass.SymAttribute.PREL31_VALUE),
        TARGET1(SymbolicExpressionOuterClass.SymAttribute.TARGET1_VALUE),
        TARGET2(SymbolicExpressionOuterClass.SymAttribute.TARGET2_VALUE),
        SBREL(SymbolicExpressionOuterClass.SymAttribute.SBREL_VALUE),
        TLSLDO(SymbolicExpressionOuterClass.SymAttribute.TLSLDO_VALUE),
        HI16(SymbolicExpressionOuterClass.SymAttribute.HI16_VALUE),
        LO16(SymbolicExpressionOuterClass.SymAttribute.LO16_VALUE),
        GPREL(SymbolicExpressionOuterClass.SymAttribute.GPREL_VALUE),
        DISP(SymbolicExpressionOuterClass.SymAttribute.DISP_VALUE),
        OFST(SymbolicExpressionOuterClass.SymAttribute.OFST_VALUE),
        H(SymbolicExpressionOuterClass.SymAttribute.H_VALUE),
        L(SymbolicExpressionOuterClass.SymAttribute.L_VALUE),
        HA(SymbolicExpressionOuterClass.SymAttribute.HA_VALUE),
        HIGH(SymbolicExpressionOuterClass.SymAttribute.HIGH_VALUE),
        HIGHA(SymbolicExpressionOuterClass.SymAttribute.HIGHA_VALUE),
        HIGHERA(SymbolicExpressionOuterClass.SymAttribute.HIGHERA_VALUE),
        HIGHESTA(SymbolicExpressionOuterClass.SymAttribute.HIGHESTA_VALUE),
        TOCBASE(SymbolicExpressionOuterClass.SymAttribute.TOCBASE_VALUE),
        TOC(SymbolicExpressionOuterClass.SymAttribute.TOC_VALUE),
        NOTOC(SymbolicExpressionOuterClass.SymAttribute.NOTOC_VALUE),
        ;

        private final int value;
        private static final Map<Integer, AttributeFlag> mapping = initMap();

        AttributeFlag(int value) { this.value = value; }

        public int value() { return this.value; }

        public static AttributeFlag fromInteger(int value) {
            return mapping.get(value);
        }

        private static Map<Integer, AttributeFlag> initMap() {
            Map<Integer, AttributeFlag> mapping = new HashMap<>();
            for (AttributeFlag flag : AttributeFlag.values()) {
                mapping.put(flag.value(), flag);
            }
            return Collections.unmodifiableMap(mapping);
        }
    }

    private Set<AttributeFlag> attributeFlags;
    private Set<Integer> unknownAttributeFlags;

    /**
     * Class constructor for a SymbolicExpression from a protobuf symbolic
     * expression.
     * @param  protoSymbolicExpression     The symbolic expression as serialized
     * into a protocol buffer.
     */
    protected SymbolicExpression(SymbolicExpressionOuterClass.SymbolicExpression
                                     protoSymbolicExpression) {
        this.attributeFlags = new HashSet<AttributeFlag>();
        this.unknownAttributeFlags = new HashSet<Integer>();

        for (Integer value :
             protoSymbolicExpression.getAttributeFlagsValueList()) {
            AttributeFlag attributeFlag = AttributeFlag.fromInteger(value);
            if (attributeFlag == null) {
                this.unknownAttributeFlags.add(value);
            } else {
                this.attributeFlags.add(attributeFlag);
            }
        }
    }

    /**
     * Class constructor for a SymbolicExpression.
     * @param  attributeFlags A set of flags that are applicable to this
     *                        symbolic expression.
     */
    protected SymbolicExpression(List<AttributeFlag> attributeFlags) {
        this.attributeFlags = new HashSet<AttributeFlag>();
        for (AttributeFlag attributeFlag : attributeFlags)
            this.addAttributeFlag(attributeFlag);
        this.unknownAttributeFlags = new HashSet<Integer>();
    }

    /**
     * Get the flags applying to this SymbolicExpression.
     *
     * @return  An unmodifiable {@link AttributeFlag} set of all the
     * attribute flags in this {@link SymbolicExpresion}.
     */
    public Set<AttributeFlag> getAttributeFlags() {
        return Collections.unmodifiableSet(this.attributeFlags);
    }

    /**
     * Add an attribute flags to this SymbolicExpression.
     *
     * @param attributeFlag An {@link AttributeFlag} that will be applied
     * to this symbolic expression.
     */
    public void addAttributeFlag(AttributeFlag attributeFlag) {
        this.attributeFlags.add(attributeFlag);
    }

    /**
     * Remove an attribute flags from this SymbolicExpression.
     *
     * @param attributeFlag An {@link AttributeFlag} that will be removed
     * from this symbolic expression.
     * @return boolean true if this symbolic expression contained the attribute
     * flag, and it was removed.
     */
    public boolean removeAttributeFlag(AttributeFlag attributeFlag) {
        return (this.attributeFlags.remove(attributeFlag));
    }

    /**
     * Get unknown attribute flags applying to this SymbolicExpression.
     *
     * @return  An unmodifiable list of all the unknown attribute
     * flags in this {@link SymbolicExpresion}, as integers.
     */
    public Set<Integer> getUnknownAttributeFlags() {
        return Collections.unmodifiableSet(this.unknownAttributeFlags);
    }

    /**
     * Add an unknown attribute flag to this SymbolicExpression.
     *
     * @param Integer An unknown attribute flag that will be applied
     * to this symbolic expression and stored as an integer.
     */
    public void addUnknownFlag(Integer unknownFlag) {
        this.unknownAttributeFlags.add(unknownFlag);
    }

    /**
     * Remove an unknown attribute flag from this SymbolicExpression.
     *
     * @param Integer An unknown attribute flag that will be removed
     * to this symbolic expression.
     * @return boolean true if this symbolic expression contained the attribute
     * flag, and it was removed.
     */
    public boolean removeUnknownFlag(Integer unknownFlag) {
        return (this.unknownAttributeFlags.remove(unknownFlag));
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
