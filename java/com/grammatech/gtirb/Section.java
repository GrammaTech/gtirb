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

import com.grammatech.gtirb.proto.SectionOuterClass;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class Section extends Node {
    public enum SectionFlag {
        Undefined(com.grammatech.gtirb.proto.SectionOuterClass.SectionFlag
                      .Section_Undefined_VALUE),
        Readable(com.grammatech.gtirb.proto.SectionOuterClass.SectionFlag
                     .Readable_VALUE),
        Writable(com.grammatech.gtirb.proto.SectionOuterClass.SectionFlag
                     .Writable_VALUE),
        Executable(com.grammatech.gtirb.proto.SectionOuterClass.SectionFlag
                       .Executable_VALUE),
        Loaded(com.grammatech.gtirb.proto.SectionOuterClass.SectionFlag
                   .Loaded_VALUE),
        Initialized(com.grammatech.gtirb.proto.SectionOuterClass.SectionFlag
                        .Initialized_VALUE),
        ThreadLocal(com.grammatech.gtirb.proto.SectionOuterClass.SectionFlag
                        .ThreadLocal_VALUE);

        private int value;

        private SectionFlag(int value) { this.value = value; }
    }

    private String name;
    private List<ByteInterval> byteIntervals;
    private List<SectionFlag> sectionFlags;
    private long elfSectionFlags;
    private long elfSectionType;
    private com.grammatech.gtirb.proto.SectionOuterClass.Section protoSection;

    public Section(
        com.grammatech.gtirb.proto.SectionOuterClass.Section protoSection) {

        this.protoSection = protoSection;
        UUID myUuid = Util.byteStringToUuid(protoSection.getUuid());
        super.setUuid(myUuid);
        this.name = protoSection.getName();

        this.byteIntervals = new ArrayList<ByteInterval>();
        List<com.grammatech.gtirb.proto.ByteIntervalOuterClass.ByteInterval>
            protoByteIntervalList = protoSection.getByteIntervalsList();
        for (com.grammatech.gtirb.proto.ByteIntervalOuterClass
                 .ByteInterval protoByteInterval : protoByteIntervalList) {
            ByteInterval newByteInterval = new ByteInterval(protoByteInterval);
            this.byteIntervals.add(newByteInterval);
        }

        this.sectionFlags = new ArrayList<SectionFlag>();
        List<com.grammatech.gtirb.proto.SectionOuterClass.SectionFlag>
            protoSectionFlagList = protoSection.getSectionFlagsList();
        for (com.grammatech.gtirb.proto.SectionOuterClass
                 .SectionFlag protoSectionFlag : protoSectionFlagList) {
            SectionFlag newSectionFlag =
                SectionFlag.values()[protoSectionFlag.getNumber()];
            this.sectionFlags.add(newSectionFlag);
        }
        // If this is an ELF file, section type and flags may be coming from the
        // AuxData elfSectionProperties, use these if that is the case
        this.elfSectionType = 0;
        this.elfSectionFlags = 0;
    }

    public String getName() { return name; }

    public List<ByteInterval> getByteIntervals() { return byteIntervals; }

    public List<SectionFlag> getSectionFlags() { return sectionFlags; }

    public void setElfSectionType(long sectionType) {
        this.elfSectionType = sectionType;
    }

    public long getElfSectionType() { return this.elfSectionType; }

    public void setElfSectionFlags(long sectionFlags) {
        this.elfSectionFlags = sectionFlags;
    }

    public long getElfSectionFlags() { return this.elfSectionFlags; }

    public SectionOuterClass.Section getProtoSection() {
        return this.protoSection;
    }
}
