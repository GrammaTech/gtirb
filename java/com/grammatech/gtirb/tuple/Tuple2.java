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

package com.grammatech.gtirb.tuple;

/**
 * Immutable collection of two elements of any type.
 *
 * Note that this class is abstract. Intended use is to extend this
 * class with a simple wrapper that uses named getters/setters.
 */
public abstract class Tuple2<A, B> {

    private final A first;
    private final B second;

    /**
     * Class constructor for a Tuple2. Once created, a tuple cannot be
     * modified.
     * @param  first  The first element of the tuple.
     * @param  second  The second element of the tuple.
     */
    public Tuple2(A first, B second) {
        this.first = first;
        this.second = second;
    }

    /**
     * Get the first element of the {@link Tuple2}.
     *
     * @return  The first element.
     */
    public A get0() { return this.first; }

    /**
     * Get the second element of the {@link Tuple2}.
     *
     * @return  The second element.
     */
    public B get1() { return this.second; }

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

        if (!(other instanceof Tuple2<?, ?>)) {
            return false;
        }

        Tuple2<?, ?> asTuple2 = (Tuple2<?, ?>)other;
        return this.first.equals(asTuple2.first) &&
            this.second.equals(asTuple2.second);
    }
}
