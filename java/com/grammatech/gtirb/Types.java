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
 * Schema class for functionBlocks auxiliary data.
 * Key: The UUID of a {@link DataBlock}.
 * Value: The type of the data, expressed as a string containing a C++ type
 * specifier. Attached To: Module
 */
public class Types {
    private Map<UUID, String> map;

    /**
     * Class Constructor.
     * @param  map  The map of DataBlock UUIDs to types.
     */
    public Types(Map<UUID, String> map) { this.map = map; }

    /**
     * Get a type.
     *
     * @param uuid The UUID of data block.
     * @return  A type.
     */
    public String getTypes(UUID uuid) { return this.map.get(uuid); }

    /**
     * Set function blocks for a function.
     *
     * @param uuid The UUID of a data block.
     * @param type A type.
     */
    public void setTypes(UUID uuid, String type) { this.map.put(uuid, type); }

    /**
     * Get the function blocks map.
     *
     * @return  The map of DataBlock UUIDs to types.
     */
    public Map<UUID, String> getMap() { return this.map; }

    /**
     * Set the function blocks map.
     *
     * @param  map  The map of DataBlock UUIDs to types.
     */
    public void setMap(Map<UUID, String> map) { this.map = map; }
}
