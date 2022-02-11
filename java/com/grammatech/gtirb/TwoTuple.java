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

import java.util.AbstractList;

/**
 * Immutable collection of two elements of any type.
 */
public class TwoTuple<S1, S2> extends AbstractList<Object> {

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
     * Get the size of this tuple. Every TwoTuple has size 2.
     * @return Tuple size
     */
    public int size() { return 2; }
}
