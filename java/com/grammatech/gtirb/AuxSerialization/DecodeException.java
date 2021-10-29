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

package com.grammatech.gtirb.AuxSerialization;

/**
 * An exception during decoding.
 */
public class DecodeException extends RuntimeException {

    /**
     * Auto-generated default serial version iD.
     */
    private static final long serialVersionUID = 1L;

    public DecodeException(String errorMessage) { super(errorMessage); }
}
