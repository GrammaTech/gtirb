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
