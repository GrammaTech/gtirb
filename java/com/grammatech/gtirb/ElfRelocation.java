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

// Properly, this is an ELF X86_64 relocation, and the name should reflect that.

public class ElfRelocation {

    public enum RelocationSectionType {
        REL,
        RELA;
    }

    public enum RelocType {
        R_X86_64_NONE(0),
        R_X86_64_64(1),
        R_X86_64_PC32(2),
        R_X86_64_GOT32(3),
        R_X86_64_PLT32(4),
        R_X86_64_COPY(5),
        R_X86_64_GLOB_DAT(6),
        R_X86_64_JUMP_SLOT(7),
        R_X86_64_RELATIVE(8),
        R_X86_64_GOTPCREL(9);
        private int value;

        private RelocType(int value) { this.value = value; }
    }

    private RelocType relocType;
    private int relocSym;
    private long relocAddr;
    private long relocAddend;

    public ElfRelocation(long offset, long info, long addend,
                         long imageLoadAddress) {

        this.setRelocType(RelocType.values()[(int)(info & 0x0ffffffff)]);
        this.setRelocSym((int)(info >>> 32));
        this.setRelocAddr(offset + imageLoadAddress);
        this.setRelocAddend(addend);
    }

    public ElfRelocation(long offset, long info, long imageLoadAddress) {

        this.setRelocType(RelocType.values()[(int)(info & 0x0ffffffff)]);
        this.setRelocSym((int)(info >>> 32));
        this.setRelocAddr(offset + imageLoadAddress);
        this.setRelocAddend(0);
    }

    public RelocType getRelocType() { return relocType; }

    public void setRelocType(RelocType relocType) {
        this.relocType = relocType;
    }

    public int getRelocSym() { return relocSym; }

    public void setRelocSym(int relocSym) { this.relocSym = relocSym; }

    public long getRelocAddr() { return relocAddr; }

    public void setRelocAddr(long relocAddr) { this.relocAddr = relocAddr; }

    public long getRelocAddend() { return relocAddend; }

    public void setRelocAddend(long relocAddend) {
        this.relocAddend = relocAddend;
    }
}
