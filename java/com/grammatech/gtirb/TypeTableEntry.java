package com.grammatech.gtirb;

import com.grammatech.gtirb.tuple.Tuple1;
import com.grammatech.gtirb.tuple.Tuple2;
import com.grammatech.gtirb.variant.Token;
import com.grammatech.gtirb.variant.Variant11;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

/**
 * A variant representing type information for an object.
 */
public class TypeTableEntry
    extends Variant11<Long, TypeTableEntry.BoolType, TypeTableEntry.IntType,
                      Long, Long, TypeTableEntry.FunctionType, UUID,
                      TypeTableEntry.ArrayType, UUID, TypeTableEntry.StructType,
                      TypeTableEntry.VoidType> {

    /**
     * A marker type representing the type bool.
     */
    public static class BoolType extends Tuple1<Byte> {
        /**
         * Constructor.
         *
         * Note that this tuple's byte is defined to always be 0.
         */
        public BoolType(Byte b) { super((byte)0); }
    }

    /**
     * A marker type representing the type int.
     */
    public static class IntType extends Tuple2<Byte, Long> {
        /**
         * Constructor.
         *
         * @param signedness Whether or not the type is signed (1 - signed, 0 -
         *     unsigned).
         * @param width Number of bytes the type is implemented with.
         */
        public IntType(Byte signedness, Long width) {
            super(signedness, width);
        }

        /**
         * True if the type is signed.
         */
        public boolean isSigned() { return this.get0() == 1; }

        /**
         * Get the width.
         */
        public Long getWidth() { return this.get1(); }
    }

    /**
     * A marker type for representing function types.
     */
    public static class FunctionType extends Tuple2<UUID, List<UUID>> {
        /**
         * Constructor.
         *
         * @param returnType The UUID of the return type.
         * @param paramTypes The types of the parameters.
         */
        public FunctionType(UUID returnType, List<UUID> paramTypes) {
            super(returnType, paramTypes);
        }

        /**
         * Get the return type UUID.
         */
        public UUID getReturnType() { return this.get0(); }

        /**
         * Get the types of the parameters.
         */
        public List<UUID> getParamTypes() { return this.get1(); }
    }

    /**
     * A marker type for representing array types.
     */
    public static class ArrayType extends Tuple2<UUID, Long> {
        /**
         * Constructor.
         *
         * @param elemType The type of the elements of the array.
         * @param size The size of the array.
         */
        public ArrayType(UUID elemType, Long size) { super(elemType, size); }

        /**
         * Get the element type UUID.
         */
        public UUID getElemType() { return this.get0(); }

        /**
         * Get the size of the array type.
         */
        public Long getSize() { return this.get1(); }
    }

    /**
     * A tuple representing a structure field.
     */
    public static class StructField extends Tuple2<Long, UUID> {
        /**
         * Constructor.
         *
         * @param offset The offset within the struct this field is at.
         * @param type The UUID of the type stored in this field.
         */
        public StructField(Long offset, UUID type) { super(offset, type); }

        /**
         * Get the offset of the field.
         */
        public Long getOffset() { return this.get0(); }

        /**
         * Get the type of the field.
         */
        public UUID getType() { return this.get1(); }
    }

    /**
     * A marker type for representing structure types.
     */
    public static class StructType extends Tuple2<Long, List<StructField>> {
        /**
         * Constructor.
         *
         * @param size The total size of the struct.
         * @param fields The list of fields in the struct.
         */
        public StructType(Long size, List<StructField> fields) {
            super(size, fields);
        }

        /**
         * Get the total size of the struct.
         */
        public Long getSize() { return this.get0(); }

        /**
         * Get the fields of the struct.
         */
        public List<StructField> getFields() { return this.get1(); }
    }

    /**
     * A marker type representing the type void.
     */
    public static class VoidType extends Tuple1<Byte> {
        /**
         * Constructor.
         *
         * Note that this tuple's byte is defined to always be 0.
         */
        public VoidType(Byte b) { super((byte)0); }
    }

    /* Hidden constructors */
    private TypeTableEntry(Token.T0 tok, Long v) { super(tok, v); }
    private TypeTableEntry(Token.T1 tok, BoolType v) { super(tok, v); }
    private TypeTableEntry(Token.T2 tok, IntType v) { super(tok, v); }
    private TypeTableEntry(Token.T3 tok, Long v) { super(tok, v); }
    private TypeTableEntry(Token.T4 tok, Long v) { super(tok, v); }
    private TypeTableEntry(Token.T5 tok, FunctionType v) { super(tok, v); }
    private TypeTableEntry(Token.T6 tok, UUID v) { super(tok, v); }
    private TypeTableEntry(Token.T7 tok, ArrayType v) { super(tok, v); }
    private TypeTableEntry(Token.T8 tok, UUID v) { super(tok, v); }
    private TypeTableEntry(Token.T9 tok, StructType v) { super(tok, v); }
    private TypeTableEntry(Token.T10 tok, VoidType v) { super(tok, v); }

    /**
     * Get the size of an unknown type.
     */
    public Optional<Long> getSizeIfUnknown() { return this.get0(); }

    /**
     * Returns whether or not the type is a bool type.
     */
    public boolean isBool() { return this.get1().isPresent(); }

    /**
     * Get the IntType tuple if this is an int type.
     */
    public Optional<IntType> getAsInt() { return this.get2(); }

    /**
     * Get the size of a char type.
     */
    public Optional<Long> getSizeIfChar() { return this.get3(); }

    /**
     * Get the size of a float type.
     */
    public Optional<Long> getSizeIfFloat() { return this.get4(); }

    /**
     * Get the FunctionType tuple if this is a function type.
     */
    public Optional<FunctionType> getAsFunction() { return this.get5(); }

    /**
     * Get the UUID of the pointed-to type if this is a pointer type.
     */
    public Optional<UUID> getUuidIfPointer() { return this.get6(); }

    /**
     * Get the ArrayType tuple if this is an array type.
     */
    public Optional<ArrayType> getAsArray() { return this.get7(); }

    /**
     * Get the UUID of the aliased type if this is an alias type.
     */
    public Optional<UUID> getUuidIfAlias() { return this.get8(); }

    /**
     * Get the StructType tuple if this is a struct type.
     */
    public Optional<StructType> getAsStruct() { return this.get9(); }

    /**
     * Returns true if this is a void type.
     */
    public boolean isVoid() { return this.get10().isPresent(); }

    /**
     * Factory for unknown type.
     */
    public static TypeTableEntry makeUnknown(Long size) {
        return new TypeTableEntry(new Token.T0(), size);
    }

    /**
     * Factory for bool type.
     */
    public static TypeTableEntry makeBool(BoolType boolType) {
        return new TypeTableEntry(new Token.T1(), boolType);
    }

    /**
     * Factory for int type.
     */
    public static TypeTableEntry makeInt(IntType intType) {
        return new TypeTableEntry(new Token.T2(), intType);
    }

    /**
     * Factory for char type.
     */
    public static TypeTableEntry makeChar(Long size) {
        return new TypeTableEntry(new Token.T3(), size);
    }

    /**
     * Factory for float type.
     */
    public static TypeTableEntry makeFloat(Long size) {
        return new TypeTableEntry(new Token.T4(), size);
    }

    /**
     * Factory for function type.
     */
    public static TypeTableEntry makeFunction(FunctionType funcType) {
        return new TypeTableEntry(new Token.T5(), funcType);
    }

    /**
     * Factory for pointer type.
     */
    public static TypeTableEntry makePointer(UUID pointedToType) {
        return new TypeTableEntry(new Token.T6(), pointedToType);
    }

    /**
     * Factory for array type.
     */
    public static TypeTableEntry makeArray(ArrayType arrType) {
        return new TypeTableEntry(new Token.T7(), arrType);
    }

    /**
     * Factory for alias type.
     */
    public static TypeTableEntry makeAlias(UUID aliasedType) {
        return new TypeTableEntry(new Token.T8(), aliasedType);
    }

    /**
     * Factory for struct type.
     */
    public static TypeTableEntry makeStruct(StructType structType) {
        return new TypeTableEntry(new Token.T9(), structType);
    }

    /**
     * Factory for void type.
     */
    public static TypeTableEntry makeVoid(VoidType voidType) {
        return new TypeTableEntry(new Token.T10(), voidType);
    }
}
