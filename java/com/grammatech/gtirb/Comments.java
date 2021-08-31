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
 * Schema class for comments auxiliary data.
 * Key: The {@link Offset} of a comment.
 * Value: A comment string relevant to the specified offset in the specified
 * GTIRB entry. Attached To: Module
 */
public class Comments {
    private Map<Offset, String> map;

    /**
     * Class Constructor.
     * @param  map  The map of Offsets to comment strings.
     */
    public Comments(Map<Offset, String> map) { this.map = map; }

    /**
     * Get a comment.
     *
     * @param {@link Offset} of a comment.
     * @return  The comment.
     */
    public String getComment(Offset uuid) { return this.map.get(uuid); }

    /**
     * Set a comment.
     *
     * @param offset   The {@link Offset} of a comment.
     * @param comment  The comment.
     */
    public void setComments(Offset offset, String comment) {
        this.map.put(offset, comment);
    }

    /**
     * Get the function blocks map.
     *
     * @return  The map of Offsets to comment strings.
     */
    public Map<Offset, String> getMap() { return this.map; }

    /**
     * Set the function blocks map.
     *
     * @param  map  The map of Offsets to comment strings.
     */
    public void setMap(Map<Offset, String> map) { this.map = map; }
}
