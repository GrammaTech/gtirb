package com.grammatech.gtirb;

import java.util.Iterator;

/**
 * Immutable collection of two elements of any type.
 */
public class TwoTuple<S1, S2> implements Iterable<Object> {

    private final S1 first;
    private final S2 second;

    /**
     * Class constructor for a TwoTuple. Once created, a tuple cannot be
     * modified.
     * @param  first  The first element of the tuple.
     * @param  second  The second element of the tuple.
     */
    public TwoTuple(S1 first, S2 second) {
        this.first = first;
        this.second = second;
    }

    /**
     * Get the first element of the {@link TwoTuple}.
     *
     * @return  The first element.
     */
    public S1 getFirst() { return this.first; }

    /**
     * Get the second element of the {@link TwoTuple}.
     *
     * @return  The second element.
     */
    public S2 getSecond() { return this.second; }

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
        return null;
    }

    /**
     * Get an iterator for the {@link TwoTuple}.
     *
     * @return  An iterator.
     */
    @Override
    public Iterator<Object> iterator() {
        Iterator<Object> it = new Iterator<Object>() {
            private int currentIndex = 0;

            @Override
            public boolean hasNext() {
                if (currentIndex < 2)
                    return true;
                return false;
            }

            @Override
            public Object next() {
                if (currentIndex++ == 0)
                    return first;
                else if (currentIndex++ == 1)
                    return second;
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
