/** */
package com.grammatech.gtirb;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.UUID;

public class Node {
    UUID uuid;
    private static HashMap<UUID, WeakReference<Node>> uuid_cache =
        new HashMap<UUID, WeakReference<Node>>();

    public Node() {
        // TODO: Assign a new UUID here.
        // (However, so far I am only working with objects that have already
        // been created,
        //  so would only have to be replaced with the correct UUID.)
    }

    public static Node getByUuid(UUID uuid) {
        return uuid_cache.get(uuid).get();
    }

    public WeakReference<Node> setUuid(UUID uuid) {
        this.uuid = uuid;
        return uuid_cache.put(uuid, new WeakReference<Node>(this));
    }

    public UUID getUuid() { return uuid; }
}
