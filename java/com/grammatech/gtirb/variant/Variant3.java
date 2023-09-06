package com.grammatech.gtirb.variant;

import java.util.Optional;

/**
 * A 3-valued variant. The variant can store a single object that is
 * of one of the generic parameter types provided.
 *
 * Note that this class is abstract. Intended use is to extend this
 * class with a simple wrapper that uses named getters/setters.
 */
public abstract class Variant3<A, B, C> {
    private Object o;
    private int index;

    /**
     * Initialize the variant with an object of the first type.
     *
     * @param tok This is a marker parameter indicating which
     * field of the variant is to be populated by the constructor.
     * @param o The object to populate the variant with.
     */
    protected Variant3(Token.T0 tok, A o) {
        this.index = 0;
        this.o = o;
    }

    /**
     * Initialize the variant with an object of the second type.
     *
     * @param tok This is a marker parameter indicating which
     * field of the variant is to be populated by the constructor.
     * @param o The object to populate the variant with.
     */
    protected Variant3(Token.T1 tok, B o) {
        this.index = 1;
        this.o = o;
    }

    /**
     * Initialize the variant with an object of the third type.
     *
     * @param tok This is a marker parameter indicating which
     * field of the variant is to be populated by the constructor.
     * @param o The object to populate the variant with.
     */
    protected Variant3(Token.T2 tok, C o) {
        this.index = 2;
        this.o = o;
    }

    /**
     * Get the field index that is populated in this variant.
     *
     * @return The index (0-based) of the field of the variant.
     */
    public int getIndex() { return this.index; }

    /**
     * Get the object in the first field of the variant.
     *
     * @return If the field in the variant is populated, the return
     * value is the populated object wrapped in Optional. Otherwise
     * returns Optional.empty().
     */
    public Optional<A> get0() {
        if (this.index == 0) {
            @SuppressWarnings("unchecked") A a = (A)o;
            return Optional.of(a);
        } else {
            return Optional.empty();
        }
    }

    /**
     * Get the object in the second field of the variant.
     *
     * @return If the field in the variant is populated, the return
     * value is the populated object wrapped in Optional. Otherwise
     * returns Optional.empty().
     */
    public Optional<B> get1() {
        if (this.index == 1) {
            @SuppressWarnings("unchecked") B b = (B)o;
            return Optional.of(b);
        } else {
            return Optional.empty();
        }
    }

    /**
     * Get the object in the third field of the variant.
     *
     * @return If the field in the variant is populated, the return
     * value is the populated object wrapped in Optional. Otherwise
     * returns Optional.empty().
     */
    public Optional<C> get2() {
        if (this.index == 2) {
            @SuppressWarnings("unchecked") C c = (C)o;
            return Optional.of(c);
        } else {
            return Optional.empty();
        }
    }

    /**
     * Set the first field of the variant. If any other field
     * is populated, that field is dropped.
     *
     * @param o The object to populate the field with.
     */
    public void set0(A o) {
        this.index = 0;
        this.o = o;
    }

    /**
     * Set the second field of the variant. If any other field
     * is populated, that field is dropped.
     *
     * @param o The object to populate the field with.
     */
    public void set1(B o) {
        this.index = 1;
        this.o = o;
    }

    /**
     * Set the third field of the variant. If any other field
     * is populated, that field is dropped.
     *
     * @param o The object to populate the field with.
     */
    public void set2(C o) {
        this.index = 2;
        this.o = o;
    }

    /**
     * Implementation of deep equality.
     * @param other The other object to compare equality against
     * @return True of other is equal to this object.
     */
    @Override
    public boolean equals(Object other) {
        if (other == this) {
            return true;
        }

        if (!(other instanceof Variant3<?, ?, ?>)) {
            return false;
        }

        Variant3<?, ?, ?> asVariant = (Variant3<?, ?, ?>)other;
        return this.index == asVariant.index && this.o.equals(asVariant.o);
    }
}
