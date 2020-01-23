package com.grammatech.gtirb;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class Section extends Node {
    enum SectionFlag {
        Undefined(com.grammatech.gtirb.proto.SectionOuterClass.SectionFlag
                      .Undefined_VALUE),
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
    ;

    private String name;
    private List<ByteInterval> byteIntervals;
    private List<SectionFlag> sectionFlags;

    public Section(
        com.grammatech.gtirb.proto.SectionOuterClass.Section protoSection) {
        UUID uuid = Util.byteStringToUuid(protoSection.getUuid());
        super.setUuid(uuid);
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
    }

    public String getName() { return name; }

    public List<ByteInterval> getByteIntervals() { return byteIntervals; }

    public List<SectionFlag> getSectionFlags() { return sectionFlags; }
}
