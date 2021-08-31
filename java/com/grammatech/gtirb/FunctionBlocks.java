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
import java.util.Set;
import java.util.UUID;

/**
 * Schema class for functionBlocks auxiliary data.
 * Key: Function UUID.
 * Value: The set of UUIDs of all the CodeBlocks in the function.
 * Attached To: Module
 */
public class FunctionBlocks {
    private Map<UUID, Set<UUID>> map;

    /**
     * Class Constructor.
     * @param  map  The map of function UUIDs to CodeBlock UUIDs in the
     * function.
     */
    public FunctionBlocks(Map<UUID, Set<UUID>> map) { this.map = map; }

    /**
     * Get function blocks for a function.
     *
     * @param uuid The UUID of a function.
     * @return  A set of {@link CodeBlock} UUIDs.
     */
    public Set<UUID> getFunctionBlocks(UUID uuid) { return this.map.get(uuid); }

    /**
     * Set function blocks for a function.
     *
     * @param uuid    The UUID of a function.
     * @param blocks  A set of {@link CodeBlock} UUIDs.
     */
    public void setFunctionBlocks(UUID uuid, Set<UUID> blocks) {
        this.map.put(uuid, blocks);
    }

    /**
     * Get the function blocks map.
     *
     * @return  A map of function UUIDs to sets of {@link CodeBlock} UUIDs.
     */
    public Map<UUID, Set<UUID>> getMap() { return this.map; }

    /**
     * Set the function blocks map.
     *
     * @param map  A map of function UUIDs to sets of {@link CodeBlock} UUIDs.
     */
    public void setMap(Map<UUID, Set<UUID>> map) { this.map = map; }
}
