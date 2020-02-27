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

import java.util.UUID;

public class Offset {

    private UUID elementId;
    private long displacement;

    public Offset(UUID elementId, long displacement) {
        this.elementId = elementId;
        this.displacement = displacement;
    }

    public UUID getElementId() { return this.elementId; }

    public long getDisplacement() { return this.displacement; }
}
