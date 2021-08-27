package com.grammatech.gtirb;

import java.util.Iterator;

/**
 * Immutable collection of three elements of any type.
 */
public class ThreeTuple<S1, S2, S3> implements Iterable<Object> {

    private final S1 first;
    private final S2 second;
    private final S3 third;

    /**
     * Class constructor for a ThreeTuple. Once created, a tuple cannot be
     * modified.
     * @param  first  The first element of the tuple.
     * @param  second  The second element of the tuple.
     * @param  third  The third element of the tuple.
     */
    public ThreeTuple(S1 first, S2 second, S3 third) {
        this.first = first;
        this.second = second;
        this.third = third;
    }

    /**
     * Get the first element of the {@link ThreeTuple}.
     *
     * @return  The first element.
     */
    public S1 getFirst() { return this.first; }

    /**
     * Get the second element of the {@link ThreeTuple}.
     *
     * @return  The second element.
     */
    public S2 getSecond() { return this.second; }

    /**
     * Get the third element of the {@link ThreeTuple}.
     *
     * @return  The third element.
     */
    public S3 getThird() { return this.third; }

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
        return null;
    }

    /**
     * Get an iterator for the {@link ThreeTuple}.
     *
     * @return  An iterator.
     */
    @Override
    public Iterator<Object> iterator() {
        Iterator<Object> it = new Iterator<Object>() {
            private int currentIndex = 0;

            @Override
            public boolean hasNext() {
                if (currentIndex < 3)
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
