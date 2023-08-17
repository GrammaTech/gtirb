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
 */
public class Pair<A, B> {

    private final A first;
    private final B second;

    /**
     * Class constructor for a Pair. Once created, a tuple cannot be
     * modified.
     * @param  first  The first element of the tuple.
     * @param  second  The second element of the tuple.
     */
    public Pair(A first, B second) {
        this.first = first;
        this.second = second;
    }

    /**
     * Get the first element of the {@link Pair}.
     *
     * @return  The first element.
     */
    public A getFirst() { return this.first; }

    /**
     * Get the second element of the {@link Pair}.
     *
     * @return  The second element.
     */
    public B getSecond() { return this.second; }

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

        if (!(other instanceof Pair<?, ?>)) {
            return false;
        }

        Pair<?, ?> asPair = (Pair<?, ?>)other;
        return this.first.equals(asPair.first) &&
            this.second.equals(asPair.second);
    }
}
