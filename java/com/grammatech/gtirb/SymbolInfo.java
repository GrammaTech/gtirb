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

public class SymbolInfo {

    private long size;
    private String type;
    private String binding;
    private String visibility;
    private long section;

    public SymbolInfo(long size, String type, String binding, String visibility,
                      long section) {
        this.size = size;
        this.type = type;
        this.binding = binding;
        this.visibility = visibility;
        this.section = section;
    }

    public long getSize() { return size; }

    public void setSize(long size) { this.size = size; }

    public String getType() { return type; }

    public void setType(String type) { this.type = type; }

    public String getBinding() { return binding; }

    public void setBinding(String binding) { this.binding = binding; }

    public String getVisibility() { return visibility; }

    public void setVisibility(String visibility) {
        this.visibility = visibility;
    }

    public long getSection() { return section; }

    public void setSection(long section) { this.section = section; }
}
