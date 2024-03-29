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

/**
 * Interface for generic items to be stored in sorted order.
 */
public interface TreeListItem {

    /**
     * Get the item index.
     *
     * Item index is used to store the items in sorted order.
     *
     * @return The item index..
     */
    long getIndex();

    /**
     * Get the item size.
     *
     * Item size is used to determine overlapping addresses.
     *
     * @return The item size.
     */
    long getSize();
}
