package com.grammatech.gtirb.tuple;

/**
 * Immutable collection of one element of any type.
 *
 * Note that this class is abstract. Intended use is to extend this
 * class with a simple wrapper that uses named getters/setters.
 */
public abstract class Tuple1<A> {

    private final A first;

    /**
     * Class constructor for a Tuple1. Once created, a tuple cannot be
     * modified.
     * @param  first  The first element of the tuple.
     */
    public Tuple1(A first) { this.first = first; }

    /**
     * Get the first element of the {@link Tuple1}.
     *
     * @return  The first element.
     */
    public A get0() { return this.first; }

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

        if (!(other instanceof Tuple1<?>)) {
            return false;
        }

        Tuple1<?> asTuple1 = (Tuple1<?>)other;
        return this.first.equals(asTuple1.first);
    }
}
