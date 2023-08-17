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
public class Quadruple<A, B, C, D> {

    private final A first;
    private final B second;
    private final C third;
    private final D fourth;

    /**
     * Class constructor for a Quadruple. Once created, a tuple cannot be
     * modified.
     * @param  first  The first element of the tuple.
     * @param  second  The second element of the tuple.
     * @param  third  The third element of the tuple.
     * @param  fourth  The fourth element of the tuple.
     */
    public Quadruple(A first, B second, C third, D fourth) {
        this.first = first;
        this.second = second;
        this.third = third;
        this.fourth = fourth;
    }

    /**
     * Get the first element of the {@link Quadruple}.
     *
     * @return  The first element.
     */
    public A getFirst() { return this.first; }

    /**
     * Get the second element of the {@link Quadruple}.
     *
     * @return  The second element.
     */
    public B getSecond() { return this.second; }

    /**
     * Get the third element of the {@link Quadruple}.
     *
     * @return  The third element.
     */
    public C getThird() { return this.third; }

    /**
     * Get the fourth element of the {@link Quadruple}.
     *
     * @return  The fourth element.
     */
    public D getFourth() { return this.fourth; }

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

        if (!(other instanceof Quadruple<?, ?, ?, ?>)) {
            return false;
        }

        Quadruple<?, ?, ?, ?> asQuadruple = (Quadruple<?, ?, ?, ?>)other;
        return this.first.equals(asQuadruple.first) &&
            this.second.equals(asQuadruple.second) &&
            this.third.equals(asQuadruple.third) &&
            this.fourth.equals(asQuadruple.fourth);
    }
}
