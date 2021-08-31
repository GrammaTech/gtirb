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

package com.grammatech.gtirb;
import java.util.Iterator;

/**
 * Immutable collection of five elements of any type.
 */
public class FiveTuple<S1, S2, S3, S4, S5> implements Iterable<Object> {

    private final S1 first;
    private final S2 second;
    private final S3 third;
    private final S4 fourth;
    private final S5 fifth;

    /**
     * Class constructor for a FiveTuple. Once created, a tuple cannot be
     * modified.
     * @param  first  The first element of the tuple.
     * @param  second  The second element of the tuple.
     * @param  third  The third element of the tuple.
     * @param  fourth  The fourth element of the tuple.
     * @param  fifth  The fifth element of the tuple.
     */
    public FiveTuple(S1 first, S2 second, S3 third, S4 fourth, S5 fifth) {
        this.first = first;
        this.second = second;
        this.third = third;
        this.fourth = fourth;
        this.fifth = fifth;
    }

    /**
     * Get the first element of the {@link FiveTuple}.
     *
     * @return  The first element.
     */
    public S1 getFirst() { return this.first; }

    /**
     * Get the second element of the {@link FiveTuple}.
     *
     * @return  The second element.
     */
    public S2 getSecond() { return this.second; }

    /**
     * Get the third element of the {@link FiveTuple}.
     *
     * @return  The third element.
     */
    public S3 getThird() { return this.third; }

    /**
     * Get the third element of the {@link FiveTuple}.
     *
     * @return  The third element.
     */
    public S4 getFourth() { return this.fourth; }

    /**
     * Get the third element of the {@link FiveTuple}.
     *
     * @return  The third element.
     */
    public S5 getFifth() { return this.fifth; }

    /**
     * Get a tuple element by index.
     *
     * @param index Which element (starting from 0).
     * @return  The element.
     */
    public Object get(int index) {
        if (index == 0)
            return this.first;
        else if (index == 1)
            return this.second;
        else if (index == 2)
            return this.third;
        else if (index == 3)
            return this.fourth;
        else if (index == 4)
            return this.fifth;
        return null;
    }

    /**
     * Get an iterator for the {@link FiveTuple}.
     *
     * @return  An iterator.
     */
    @Override
    public Iterator<Object> iterator() {
        Iterator<Object> it = new Iterator<Object>() {
            private int currentIndex = 0;

            @Override
            public boolean hasNext() {
                if (currentIndex < 5)
                    return true;
                return false;
            }

            @Override
            public Object next() {
                if (currentIndex++ == 0)
                    return first;
                else if (currentIndex++ == 1)
                    return second;
                else if (currentIndex++ == 2)
                    return third;
                else if (currentIndex++ == 3)
                    return fourth;
                else if (currentIndex++ == 4)
                    return fifth;
                // went past
                return null;
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException();
            }
        };
        return it;
    }
}
