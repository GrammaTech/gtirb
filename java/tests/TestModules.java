package tests;

import static org.junit.jupiter.api.Assertions.*;

import com.grammatech.gtirb.*;
import com.grammatech.gtirb.CodeBlock.DecodeMode;
import com.grammatech.gtirb.Module;
import com.grammatech.gtirb.Module.FileFormat;
import com.grammatech.gtirb.Module.ISA;
import com.grammatech.gtirb.Section.SectionFlag;
import java.io.File;
import java.util.*;
import org.junit.jupiter.api.Test;

public class TestModules {

    @Test
    void testModuleSaveAndLoad() throws Exception {
        // Just create a simple IR w/ 1 module
        IR ir_orig = new IR();
        Module mod =
            new Module("c:/foo.exe", 0xCAFE, 0xBEEF, Module.FileFormat.ELF,
                       Module.ISA.X64, "myModule");
        mod.setByteOrder(Module.ByteOrder.LittleEndian);
        ir_orig.addModule(mod);

        Section section =
            new Section("mySection", new HashSet<Section.SectionFlag>(),
                        new ArrayList<ByteInterval>());
        mod.addSection(section);

        Symbol symbol = new Symbol("mySymbol");
        mod.addSymbol(symbol);

        ProxyBlock proxyBlock = new ProxyBlock();
        mod.addProxyBlock(proxyBlock);

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
        assertEquals("myModule", mod_reloaded.getName());
        assertEquals(Module.FileFormat.ELF, mod_reloaded.getFileFormat());
        assertEquals(Module.ISA.X64, mod_reloaded.getIsa());
        assertEquals(Module.ByteOrder.LittleEndian,
                     mod_reloaded.getByteOrder());
        assertEquals(0xCAFE, mod_reloaded.getPreferredAddr());
        assertEquals(0xBEEF, mod_reloaded.getRebaseDelta());
        assertEquals("mySymbol", mod_reloaded.getSymbols().get(0).getName());
        assertEquals("mySection", mod_reloaded.getSections().get(0).getName());
        assertEquals(1, mod_reloaded.getProxyBlocks().size());
    }

    @Test
    void testModuleSetAndGet() throws Exception {

        CodeBlock entryPoint =
            new CodeBlock(0, 0, CodeBlock.DecodeMode.Default);
        String binaryPath = "/home/away/from/home.bin";
        String name = "myModule";
        long preferredAddr = 0xABCD;
        long rebaseDelta = 0x1234;
        FileFormat fileFormat = FileFormat.PE;
        ISA isa = ISA.IA32;

        Module module =
            new Module("/my/module", 0x0000, 0x0FFF, FileFormat.ELF, ISA.X64,
                       "module", new ArrayList<Section>(),
                       new ArrayList<Symbol>(), new ArrayList<ProxyBlock>(),
                       new CodeBlock(0, 0, DecodeMode.Default));
        assertNotNull(module);

        module.setBinaryPath(binaryPath);
        assertEquals(module.getBinaryPath(), binaryPath);

        module.setPreferredAddr(preferredAddr);
        assertEquals(module.getPreferredAddr(), preferredAddr);

        module.setRebaseDelta(rebaseDelta);
        assertEquals(module.getRebaseDelta(), rebaseDelta);

        module.setFileFormat(fileFormat);
        assertEquals(module.getFileFormat(), fileFormat);

        module.setIsa(isa);
        assertEquals(module.getIsa(), isa);

        module.setName(name);
        assertEquals(module.getName(), name);

        module.setEntryPoint(entryPoint);
        assertEquals(module.getEntryPoint(), entryPoint);
    }

    @Test
    void testAddAndRemoveSections() throws Exception {
        Module module =
            new Module("/my/module", 0x0000, 0x0FFF, Module.FileFormat.ELF,
                       Module.ISA.X64, "module");
        Section section0 =
            new Section("section0", new HashSet<Section.SectionFlag>(),
                        new ArrayList<ByteInterval>());
        Section section1 =
            new Section("section1", new HashSet<Section.SectionFlag>(),
                        new ArrayList<ByteInterval>());
        Section section2 =
            new Section("section2", new HashSet<Section.SectionFlag>(),
                        new ArrayList<ByteInterval>());

        module.addSection(section0);
        module.addSection(section1);
        module.addSection(section2);

        List<Section> sections = module.getSections();
        assertEquals(sections.size(), 3);

        assertEquals(section0.getModule(), Optional.of(module));
        assertEquals(section1.getModule(), Optional.of(module));
        assertEquals(section2.getModule(), Optional.of(module));
        module.removeSection(section0);
        module.removeSection(section1);
        assertTrue(section0.getModule().isEmpty());
        assertTrue(section1.getModule().isEmpty());
        assertEquals(section2.getModule(), Optional.of(module));

        // Now the only section left should be "section2"
        sections = module.getSections();
        assertEquals(sections.size(), 1);
        assertEquals("section2", sections.get(0).getName());
    }

    @Test
    void testAddAndRemoveSymbols() throws Exception {
        Module module =
            new Module("/my/module", 0x0000, 0x0FFF, Module.FileFormat.ELF,
                       Module.ISA.X64, "module");
        Symbol symbol0 = new Symbol("symbol0");
        Symbol symbol1 = new Symbol("symbol1");
        Symbol symbol2 = new Symbol("symbol2");

        module.addSymbol(symbol0);
        module.addSymbol(symbol2);
        module.addSymbol(symbol1);

        List<Symbol> symbols = module.getSymbols();
        assertEquals(symbols.size(), 3);

        assertEquals(symbol0.getModule(), Optional.of(module));
        assertEquals(symbol1.getModule(), Optional.of(module));
        assertEquals(symbol2.getModule(), Optional.of(module));
        module.removeSymbol(symbol2);
        module.removeSymbol(symbol0);
        assertTrue(symbol0.getModule().isEmpty());
        assertEquals(symbol1.getModule(), Optional.of(module));
        assertTrue(symbol2.getModule().isEmpty());

        // Now the only symbol left should be "symbol1"
        symbols = module.getSymbols();
        assertEquals(symbols.size(), 1);
        assertEquals("symbol1", symbols.get(0).getName());
    }

    @Test
    void testAddAndRemoveProxyBlocks() throws Exception {
        Module module =
            new Module("/my/module", 0x0000, 0x0FFF, Module.FileFormat.ELF,
                       Module.ISA.X64, "module");
        ProxyBlock proxyBlock0 = new ProxyBlock();
        ProxyBlock proxyBlock1 = new ProxyBlock();
        ProxyBlock proxyBlock2 = new ProxyBlock();

        module.addProxyBlock(proxyBlock2);
        module.addProxyBlock(proxyBlock1);
        module.addProxyBlock(proxyBlock0);

        List<ProxyBlock> proxyBlocks = module.getProxyBlocks();
        assertEquals(proxyBlocks.size(), 3);

        assertEquals(proxyBlock0.getModule(), Optional.of(module));
        assertEquals(proxyBlock1.getModule(), Optional.of(module));
        assertEquals(proxyBlock2.getModule(), Optional.of(module));
        module.removeProxyBlock(proxyBlock0);
        module.removeProxyBlock(proxyBlock2);
        module.removeProxyBlock(proxyBlock1);
        assertTrue(proxyBlock0.getModule().isEmpty());
        assertTrue(proxyBlock1.getModule().isEmpty());
        assertTrue(proxyBlock2.getModule().isEmpty());

        assertEquals(module.getProxyBlocks().size(), 0);
    }

    @Test
    void testModuleFindSections() throws Exception {

        Module module = new Module("module", 0x0000, 0x0FFF, FileFormat.ELF,
                                   ISA.X64, "module");
        Set<SectionFlag> flags = new HashSet<SectionFlag>();
        flags.add(SectionFlag.Readable);
        flags.add(SectionFlag.Writable);

        ArrayList<ByteInterval> biList1 = new ArrayList<ByteInterval>();
        biList1.add(new ByteInterval(null, 0x0));
        biList1.get(0).setSize(0x100000);
        Section section1 = new Section("Section1", flags, biList1);
        module.addSection(section1);

        ArrayList<ByteInterval> biList2 = new ArrayList<ByteInterval>();
        biList2.add(new ByteInterval(null, 0x100000));
        biList2.get(0).setSize(0x100000);
        Section section2 = new Section("Section2", flags, biList2);
        module.addSection(section2);

        ArrayList<ByteInterval> biList3 = new ArrayList<ByteInterval>();
        biList3.add(new ByteInterval(null, 0x200000));
        biList3.get(0).setSize(0x100000);
        Section section3 = new Section("Section3", flags, biList3);
        module.addSection(section3);

        // Find sections that contain an address - single
        List<Section> sectionsOn1 = module.findSectionsOn(0x7FFF);
        assertEquals(sectionsOn1.size(), 1);
        assertEquals(sectionsOn1.get(0), section1);

        // Find sections that contain an address - range
        List<Section> sectionsOn2 = module.findSectionsOn(0x180000, 0x280000);
        assertEquals(sectionsOn2.size(), 2);
        assertEquals(sectionsOn2.get(0), section2);
        assertEquals(sectionsOn2.get(1), section3);

        // Find sections that start at an address - single
        List<Section> sectionsAt1 = module.findSectionsAt(0x0);
        assertEquals(sectionsAt1.size(), 1);
        assertEquals(sectionsAt1.get(0), section1);

        // Find sections that start at an address - range
        List<Section> sectionsAt2 = module.findSectionsAt(0x180000, 0x280000);
        assertEquals(sectionsAt2.size(), 1);
        assertEquals(sectionsAt2.get(0), section3);
    }
}
