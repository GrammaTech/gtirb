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
 * Immutable collection of five elements of any type.
 *
 * Note that this class is abstract. Intended use is to extend this
 * class with a simple wrapper that uses named getters/setters.
 */
public abstract class Tuple5<A, B, C, D, E> {

    private final A first;
    private final B second;
    private final C third;
    private final D fourth;
    private final E fifth;

    /**
     * Class constructor for a Tuple5. Once created, a tuple cannot be
     * modified.
     * @param  first  The first element of the tuple.
     * @param  second  The second element of the tuple.
     * @param  third  The third element of the tuple.
     * @param  fourth  The fourth element of the tuple.
     * @param  fifth  The fifth element of the tuple.
     */
    public Tuple5(A first, B second, C third, D fourth, E fifth) {
        this.first = first;
        this.second = second;
        this.third = third;
        this.fourth = fourth;
        this.fifth = fifth;
    }

    /**
     * Get the first element of the {@link Tuple5}.
     *
     * @return  The first element.
     */
    public A get0() { return this.first; }

    /**
     * Get the second element of the {@link Tuple5}.
     *
     * @return  The second element.
     */
    public B get1() { return this.second; }

    /**
     * Get the third element of the {@link Tuple5}.
     *
     * @return  The third element.
     */
    public C get2() { return this.third; }

    /**
     * Get the fourth element of the {@link Tuple5}.
     *
     * @return  The fourth element.
     */
    public D get3() { return this.fourth; }

    /**
     * Get the fifth element of the {@link Tuple5}.
     *
     * @return  The fifth element.
     */
    public E get4() { return this.fifth; }

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

        if (!(other instanceof Tuple5<?, ?, ?, ?, ?>)) {
            return false;
        }

        Tuple5<?, ?, ?, ?, ?> asTuple5 = (Tuple5<?, ?, ?, ?, ?>)other;
        return this.first.equals(asTuple5.first) &&
            this.second.equals(asTuple5.second) &&
            this.third.equals(asTuple5.third) &&
            this.fourth.equals(asTuple5.fourth) &&
            this.fifth.equals(asTuple5.fifth);
    }
}
