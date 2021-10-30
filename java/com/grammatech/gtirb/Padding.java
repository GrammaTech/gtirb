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

import java.util.Map;

/**
 * Schema class for padding auxiliary data.
 * Key: An address ({@link Offset}) at which padding has been inserted.
 * Value: The length of the padding, in bytes.
 * Attached To: Module
 */
public class Padding {
    private Map<Offset, Long> map;

    /**
     * Class Constructor.
     * @param  map  The map of addresses to padding sizes.
     */
    public Padding(Map<Offset, Long> map) { this.map = map; }

    /**
     * Get padding at an address.
     *
     * @param offset The of address of padding
     * @return  The padding at that address, in bytes.
     */
    public long getPadding(Offset offset) { return this.map.get(offset); }

    /**
     * Set padding at an address.
     *
     * @param offset The of address of padding
     * @param padding The padding at that address, in bytes.
     */
    public void setPadding(Offset offset, long padding) {
        this.map.put(offset, padding);
    }

    /**
     * Get the padding map.
     *
     * @return  The map of addresses to padding sizes.
     */
    public Map<Offset, Long> getMap() { return this.map; }

    /**
     * Set the padding map.
     *
     * @param  map  The map of addresses to padding sizes.
     */
    public void setMap(Map<Offset, Long> map) { this.map = map; }
}
