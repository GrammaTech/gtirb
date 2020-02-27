/*
 *  Copyright (C) 2020 GrammaTech, Inc.
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
