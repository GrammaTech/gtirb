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

package com.grammatech.gtirb.tuple;

/**
 * Immutable collection of two elements of any type.
 */
public class Triple<A, B, C> {

    private final A first;
    private final B second;
    private final C third;

    /**
     * Class constructor for a Triple. Once created, a tuple cannot be
     * modified.
     * @param  first  The first element of the tuple.
     * @param  second  The second element of the tuple.
     * @param  third  The third element of the tuple.
     */
    public Triple(A first, B second, C third) {
        this.first = first;
        this.second = second;
        this.third = third;
    }

    /**
     * Get the first element of the {@link Triple}.
     *
     * @return  The first element.
     */
    public A getFirst() { return this.first; }

    /**
     * Get the second element of the {@link Triple}.
     *
     * @return  The second element.
     */
    public B getSecond() { return this.second; }

    /**
     * Get the third element of the {@link Triple}.
     *
     * @return  The third element.
     */
    public C getThird() { return this.third; }

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

        if (!(other instanceof Triple<?, ?, ?>)) {
            return false;
        }

        Triple<?, ?, ?> asTriple = (Triple<?, ?, ?>)other;
        return this.first.equals(asTriple.first) &&
            this.second.equals(asTriple.second) &&
            this.third.equals(asTriple.third);
    }
}
