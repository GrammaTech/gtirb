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

import java.util.ArrayList;
import java.util.List;

import com.grammatech.gtirb.TwoTuple;

/**
 * Intermediate product of the parsing of AuxData types
 *
 */
public class ParsedProduct {

    // This is declared as a list because
    // I am using it as a tuple.
    private List<TwoTuple<String, Object>> subtree;
    private List<String> remaining;

    public ParsedProduct(List<TwoTuple<String, Object>> subtree,
                         List<String> remaining) {
        this.setSubtree(subtree);
        this.setRemaining(remaining);
    }

    //	public ParsedProduct(TwoTuple<String,Object> element, List<String>
    // remaining) { 		this.subtree = new
    // ArrayList<TwoTuple<String,Object>>(); this.subtree.add(element);
    //		this.setRemaining(remaining);
    //	}

    /**
     * @return the subtree
     */
    public List<TwoTuple<String, Object>> getSubtree() { return subtree; }

    /**
     * @param subtree the subtree to set
     */
    public void setSubtree(List<TwoTuple<String, Object>> subtree) {
        this.subtree = subtree;
    }

    /**
     * @return the remaining
     */
    public List<String> getRemaining() { return remaining; }

    /**
     * @param remaining the remaining to set
     */
    public void setRemaining(List<String> remaining) {
        this.remaining = remaining;
    }
}
