package tests;

import static org.junit.jupiter.api.Assertions.*;

import com.grammatech.gtirb.*;
import com.grammatech.gtirb.Module;
import java.util.*;
import org.junit.jupiter.api.Test;

public class TestModules {

    @Test
    void testAddAndRemoveSections() throws Exception {
        Module module =
            new Module("/my/module", 0x0000, 0x0FFF, Module.FileFormat.ELF,
                       Module.ISA.X64, "module");
        Section section0 =
            new Section("section0", new ArrayList<Section.SectionFlag>(),
                        new ArrayList<ByteInterval>());
        Section section1 =
            new Section("section1", new ArrayList<Section.SectionFlag>(),
                        new ArrayList<ByteInterval>());
        Section section2 =
            new Section("section2", new ArrayList<Section.SectionFlag>(),
                        new ArrayList<ByteInterval>());

        module.addSection(section0);
        module.addSection(section1);
        module.addSection(section2);

        List<Section> sections = module.getSections();
        assertEquals(sections.size(), 3);

        module.removeSection(section0);
        module.removeSection(section1);

        // Now the only section left should be "section2"
        assertEquals("section2", module.getSections().get(0).getName());
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

        module.removeSymbol(symbol0);
        module.removeSymbol(symbol2);

        // Now the only symbol left should be "symbol1"
        assertEquals("symbol1", module.getSymbols().get(0).getName());
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

        module.removeProxyBlock(proxyBlock0);
        module.removeProxyBlock(proxyBlock2);
        module.removeProxyBlock(proxyBlock1);

        proxyBlocks = module.getProxyBlocks();
        assertEquals(proxyBlocks.size(), 0);
    }
}
