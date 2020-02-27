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

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class Directive {
    private String directiveString;
    private ArrayList<Long> directiveValues;
    private UUID directiveUuid;

    public Directive(String directiveString, ArrayList<Long> directiveValues,
                     UUID directiveUuid) {
        this.directiveString = directiveString;
        this.directiveValues = directiveValues;
        this.directiveUuid = directiveUuid;
    }

    public String getString() { return directiveString; }

    public void setString(String directiveString) {
        this.directiveString = directiveString;
    }

    public List<Long> getValues() { return directiveValues; }

    public void setValues(ArrayList<Long> directiveValues) {
        this.directiveValues = directiveValues;
    }

    public UUID getUuid() { return directiveUuid; }

    public void setUuid(UUID directiveUuid) {
        this.directiveUuid = directiveUuid;
    }
}
