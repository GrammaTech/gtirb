package tests;

import static org.junit.jupiter.api.Assertions.*;

import com.grammatech.gtirb.*;
import com.grammatech.gtirb.Module;
import java.io.File;
import java.util.*;
import org.junit.jupiter.api.Test;

public class TestSections {

    @Test
    void testSectionSaveAndLoad() throws Exception {
        // Just create a simple IR w/ 1 module
        IR ir_orig = new IR();
        Module mod =
            new Module("c:/foo.exe", 0xCAFE, 0xBEEF, Module.FileFormat.ELF,
                       Module.ISA.X64, "myModule");
        ir_orig.addModule(mod);

        Section section =
            new Section("mySection", new HashSet<Section.SectionFlag>(),
                        new ArrayList<ByteInterval>());
        mod.addSection(section);

        section.addSectionFlag(Section.SectionFlag.Readable);
        section.addSectionFlag(Section.SectionFlag.Writable);
        section.addSectionFlag(Section.SectionFlag.Executable);

        ByteInterval bi = new ByteInterval();
        section.addByteInterval(bi);

        section.removeSectionFlag(Section.SectionFlag.Executable);

        File file;
        file = File.createTempFile("temp", null);

        String filename = file.getName();
        try {
            ir_orig.saveFile(filename);
        } catch (Exception e) {
            file.delete();
            throw e;
        }

        IR ir_reloaded;
        try {
            ir_reloaded = IR.loadFile(filename);
        } catch (Exception e) {
            file.delete();
            throw e;
        }

        file.delete();

        assertNotNull(ir_reloaded);
        Module mod_reloaded = ir_reloaded.getModules().get(0);
        Section section_reloaded = mod_reloaded.getSections().get(0);
        Set<Section.SectionFlag> flagSet = section_reloaded.getSectionFlags();

        assertTrue(flagSet.contains(Section.SectionFlag.Readable));
        assertTrue(flagSet.contains(Section.SectionFlag.Writable));
        assertFalse(flagSet.contains(Section.SectionFlag.Executable));
        assertEquals(section_reloaded.getByteIntervals().size(), 1);
    }

    @Test
    void testSectionSetAndGet() throws Exception {
        String name = ".code";
        long address = 0x8FFFFFFF;
        long size = 0x40000;

        Section section =
            new Section("newSection", new HashSet<Section.SectionFlag>(),
                        new ArrayList<ByteInterval>());
        section.setName(name);
        assertEquals(section.getName(), name);

        ByteInterval bi = new ByteInterval();
        bi.setAddress(address);
        bi.setSize(size);

        CodeBlock b1 = new CodeBlock(4, 1, CodeBlock.DecodeMode.Default);
        bi.insertByteBlock(b1);

        section.addByteInterval(bi);

        assertEquals(section.getAddress(), OptionalLong.of(address));
        assertEquals(section.getSize(), size);
    }

    @Test
    void testSectionWithIntervalAddresses() throws Exception {
        ArrayList<ByteInterval> biList = new ArrayList<ByteInterval>();
        ByteInterval bi1 = new ByteInterval(null, 0x0);
        bi1.setSize(0x100000);
        biList.add(bi1);
        ByteInterval bi2 = new ByteInterval(null, 0x100000);
        bi2.setSize(0x100000);
        biList.add(bi2);
        ByteInterval bi3 = new ByteInterval(null, 0x200000);
        bi3.setSize(0x100000);
        biList.add(bi3);
        Section section =
            new Section("Section", new HashSet<Section.SectionFlag>(), biList);
        assertEquals(section.getAddress(), OptionalLong.of(0x0));
        assertEquals(section.getSize(), 0x300000L);

        // Find intervals that contain an address - single
        List<ByteInterval> biOn1 = section.findByteIntervalsOn(0x180000);
        assertEquals(biOn1.size(), 1);
        assertEquals(biOn1.get(0), bi2);

        // Find intervals that contain an address - range
        List<ByteInterval> biOn2 =
            section.findByteIntervalsOn(0x80000, 0x180000);
        assertEquals(biOn2.size(), 2);
        assertEquals(biOn2.get(0), bi1);
        assertEquals(biOn2.get(1), bi2);

        // Find intervals that start at an address - single
        List<ByteInterval> biAt1 = section.findByteIntervalsAt(0x100000);
        assertEquals(biAt1.size(), 1);
        assertEquals(biAt1.get(0), bi2);

        // Find intervals that start at an address - range
        List<ByteInterval> biAt2 =
            section.findByteIntervalsAt(0x80000, 0x180000);
        assertEquals(biAt2.size(), 1);
        assertEquals(biAt2.get(0), bi2);
    }

    @Test
    void testSectionNoAddressIntervals() throws Exception {
        Section section =
            new Section("aSection", new HashSet<Section.SectionFlag>(),
                        new ArrayList<ByteInterval>());

        ByteInterval bi1 = new ByteInterval(null, 0x0);
        bi1.setSize(0x100000);
        section.addByteInterval(bi1);

        // bi2 is a ByteInterval without an address, making the calculation of
        // address and size for the section impossible
        ByteInterval bi2 = new ByteInterval();
        bi2.setSize(0x100000);
        section.addByteInterval(bi2);

        ByteInterval bi3 = new ByteInterval(null, 0x400000);
        bi3.setSize(0x100000);
        section.addByteInterval(bi3);

        assertEquals(section.getAddress(), OptionalLong.empty());
        assertEquals(section.getSize(), 0L);
    }

    @Test
    void testAddAndRemoveFlags() throws Exception {
        Section section =
            new Section("section", new HashSet<Section.SectionFlag>(),
                        new ArrayList<ByteInterval>());

        section.addSectionFlag(Section.SectionFlag.ThreadLocal);
        section.addSectionFlag(Section.SectionFlag.Loaded);
        assertEquals(section.getSectionFlags().size(), 2);

        // Add same flag again should not increase list size
        section.addSectionFlag(Section.SectionFlag.Loaded);
        assertEquals(section.getSectionFlags().size(), 2);

        // Remove a flag not set should return false
        assertFalse(section.removeSectionFlag(Section.SectionFlag.Initialized));

        // Remove a flag that is set
        section.removeSectionFlag(Section.SectionFlag.ThreadLocal);
        assertEquals(section.getSectionFlags().size(), 1);
    }

    @Test
    void testAddAndRemoveByteIntervals() throws Exception {
        Section section =
            new Section("section", new HashSet<Section.SectionFlag>(),
                        new ArrayList<ByteInterval>());

        ByteInterval bi = new ByteInterval();
        assertTrue(section.getModule().isEmpty());
        assertTrue(bi.getSection().isEmpty());

        section.addByteInterval(bi);
        assertEquals(bi.getSection().get(), section);
        assertTrue(section.getByteIntervals().contains(bi));

        assertTrue(section.removeByteInterval(bi));
        assertTrue(bi.getSection().isEmpty());
        assertEquals(section.getByteIntervals().size(), 0);
    }
}
