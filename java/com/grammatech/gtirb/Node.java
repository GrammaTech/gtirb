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
    private final UUID uuid;
    private static HashMap<UUID, WeakReference<Node>> uuid_cache =
        new HashMap<UUID, WeakReference<Node>>();

    /**
     * Create a Node with a randomly generated UUID.
     */
    public Node() { this(UUID.randomUUID()); }

    /**
     * Create a Node with a specified UUID.
     */
    public Node(UUID uuid) {
        this.uuid = uuid;
        uuid_cache.put(uuid, new WeakReference<>(this));
    }

    /**
     * Find a node using its UUID.
     *
     * @return  The node with the given UUID.
     */
    public static Node getByUuid(UUID uuid) {
        WeakReference<Node> noderef = uuid_cache.get(uuid);
        if (noderef != null)
            return noderef.get();
        return null;
    }

    /**
     * Find a node of the given type by UUID.
     *
     * @return  The node with the given UUID.
     */
    public static <T extends Node> T getByUuid(UUID uuid, Class<T> type) {
        WeakReference<Node> noderef = uuid_cache.get(uuid);
        if (noderef != null) {
            Node node = noderef.get();
            if (type.isInstance(node)) {
                return type.cast(node);
            }
        }
        return null;
    }

    /**
     * Get the UUID of this node.
     *
     * @return  The UUID.
     */
    public UUID getUuid() { return uuid; }
}
