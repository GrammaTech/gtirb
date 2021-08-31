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

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.UUID;

/**
 * Node is the root class for many GTIRB components.
 */
public class Node {
    UUID uuid;
    private static HashMap<UUID, WeakReference<Node>> uuid_cache =
        new HashMap<UUID, WeakReference<Node>>();

    /**
     * Default constructor
     */
    public Node() {}

    /**
     * Find a node using its UUID.
     *
     * @return  The node with the given UUID.
     */
    public static Node getByUuid(UUID uuid) {
        return uuid_cache.get(uuid).get();
    }

    /**
     * Get the UUID of this node.
     *
     * @return  The UUID.
     */
    public UUID getUuid() { return uuid; }

    /**
     * Set the UUID of this node.
     *
     * @param uuid  The UUID.
     */
    public WeakReference<Node> setUuid(UUID uuid) {
        this.uuid = uuid;
        return uuid_cache.put(uuid, new WeakReference<Node>(this));
    }
}
