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
public class Quintuple<A, B, C, D, E> {

    private final A first;
    private final B second;
    private final C third;
    private final D fourth;
    private final E fifth;

    /**
     * Class constructor for a Quintuple. Once created, a tuple cannot be
     * modified.
     * @param  first  The first element of the tuple.
     * @param  second  The second element of the tuple.
     * @param  third  The third element of the tuple.
     * @param  fourth  The fourth element of the tuple.
     * @param  fifth  The fifth element of the tuple.
     */
    public Quintuple(A first, B second, C third, D fourth, E fifth) {
        this.first = first;
        this.second = second;
        this.third = third;
        this.fourth = fourth;
        this.fifth = fifth;
    }

    /**
     * Get the first element of the {@link Quintuple}.
     *
     * @return  The first element.
     */
    public A getFirst() { return this.first; }

    /**
     * Get the second element of the {@link Quintuple}.
     *
     * @return  The second element.
     */
    public B getSecond() { return this.second; }

    /**
     * Get the third element of the {@link Quintuple}.
     *
     * @return  The third element.
     */
    public C getThird() { return this.third; }

    /**
     * Get the fourth element of the {@link Quintuple}.
     *
     * @return  The fourth element.
     */
    public D getFourth() { return this.fourth; }

    /**
     * Get the fifth element of the {@link Quintuple}.
     *
     * @return  The fifth element.
     */
    public E getFifth() { return this.fifth; }

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

        if (!(other instanceof Quintuple<?, ?, ?, ?, ?>)) {
            return false;
        }

        Quintuple<?, ?, ?, ?, ?> asQuintuple = (Quintuple<?, ?, ?, ?, ?>)other;
        return this.first.equals(asQuintuple.first) &&
            this.second.equals(asQuintuple.second) &&
            this.third.equals(asQuintuple.third) &&
            this.fourth.equals(asQuintuple.fourth) &&
            this.fifth.equals(asQuintuple.fifth);
    }
}
