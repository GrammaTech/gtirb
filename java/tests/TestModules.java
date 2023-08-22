package tests;

import static org.junit.jupiter.api.Assertions.*;

import com.grammatech.gtirb.*;
import com.grammatech.gtirb.Module;
import com.grammatech.gtirb.Symbol;
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
}
