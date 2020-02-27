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

public class DynamicSymbol {

    public enum DynamicSymbolBinding {
        LOCAL(0),
        GLOBAL(1),
        WEAK(2);
        private int value;

        private DynamicSymbolBinding(int value) { this.value = value; }
    }

    public enum DynamicSymbolType {
        NOTYPE(0),
        OBJECT(1),
        FUNC(2),
        SECTION(3),
        FILE(4),
        COMMON(5),
        TLS(6);
        private int value;

        private DynamicSymbolType(int value) { this.value = value; }
    }

    public enum DynamicSymbolVisibility {
        DEFAULT(0),
        INTERNAL(1),
        HIDDEN(2),
        PROTECTED(3);
        private int value;

        private DynamicSymbolVisibility(int value) { this.value = value; }
    }

    private String name;
    private DynamicSymbolType type;
    private DynamicSymbolBinding binding;
    private DynamicSymbolVisibility visibility;
    private int shndx;
    private long value;
    private long size;
    private long addr;

    public DynamicSymbol(String name, byte info, byte other, short shndx,
                         long value, long size) {
        this.setName(name);
        this.setType(DynamicSymbolType.values()[info & 0x0f]);
        this.setBinding(DynamicSymbolBinding.values()[info >>> 4]);
        this.setVisibility(DynamicSymbolVisibility.values()[other & 0x03]);
        this.setShndx(shndx);
        this.setValue(value);
        this.setSize(size);
    }

    public String getName() { return name; }

    public void setName(String name) { this.name = name; }

    public DynamicSymbolType getType() { return type; }

    public void setType(DynamicSymbolType type) { this.type = type; }

    public DynamicSymbolBinding getBinding() { return binding; }

    public void setBinding(DynamicSymbolBinding binding) {
        this.binding = binding;
    }

    public DynamicSymbolVisibility getVisibility() { return visibility; }

    public void setVisibility(DynamicSymbolVisibility visibility) {
        this.visibility = visibility;
    }

    public int getShndx() { return shndx; }

    public void setShndx(int shndx) { this.shndx = shndx; }

    public long getValue() { return value; }

    public void setValue(long value) { this.value = value; }

    public long getSize() { return size; }

    public void setSize(long size) { this.size = size; }

    public long getAddr() { return addr; }

    public void setAddr(long addr) { this.addr = addr; }
}
