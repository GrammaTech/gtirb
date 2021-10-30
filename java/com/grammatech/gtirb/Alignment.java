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
import java.util.UUID;

/**
 * Schema class for alignment auxiliary data.
 * Key: The UUID of a {@link CodeBlock}, {@link DataBlock}, or {@link Section}.
 * Value: Alignment requirements for the block/data object/section.
 * Attached To: Module
 */
public class Alignment {

    private Map<UUID, Long> map;

    /**
     * Class Constructor.
     * @param  map  The map of UUIDs to alignment requirements.
     */
    public Alignment(Map<UUID, Long> map) { this.map = map; }

    /**
     * Get alignment.
     *
     * @param uuid The UUID of a block/data object/section.
     * @return  Alignment requirement.
     */
    public long getAlignment(UUID uuid) { return this.map.get(uuid); }

    /**
     * Set alignment.
     *
     * @param uuid The UUID of a block/data object/section.
     * @param alignment Alignment requirement.
     */
    public void setAlignment(UUID uuid, long alignment) {
        this.map.put(uuid, alignment);
    }

    /**
     * Get the alignment map.
     *
     * @return  A map of UUIDs to alignment requirements.
     */
    public Map<UUID, Long> getMap() { return this.map; }

    /**
     * Set the alignment map.
     *
     * @param map  A map of UUIDs to alignment requirements.
     */
    public void setMap(Map<UUID, Long> map) { this.map = map; }
}
