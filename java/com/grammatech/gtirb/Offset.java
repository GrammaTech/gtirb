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

import java.util.UUID;

/**
 * An Offset describes a location inside a {@link CodeBlock} or {@link
 * DataBlock}.
 */
public class Offset {

    private UUID elementId;
    private long displacement;

    /**
     * Constructor
     *
     * @param elementId  The UUID of a {@link ByteBlock} containing the location
     * of interest.
     * @param displacement  The offset inside the Node to point to.
     */
    public Offset(UUID elementId, long displacement) {
        this.elementId = elementId;
        this.displacement = displacement;
    }

    /**
     * Get the UUID of the block this {@link Offset} points to.
     *
     * @return  The element UUID.
     */
    public UUID getElementId() { return this.elementId; }

    /**
     * Get the displacement into the block this {@link Offset} points to.
     *
     * @return  The displacement.
     */
    public long getDisplacement() { return this.displacement; }
}
