package tests;

import static org.junit.jupiter.api.Assertions.*;

import com.grammatech.gtirb.*;
import com.grammatech.gtirb.Module;
import java.io.File;
import java.util.*;
import org.junit.jupiter.api.Test;

public class TestSymbols {

    @Test
    void testSymbolSaveAndLoad() throws Exception {
        // Just create a simple IR w/ 1 module
        IR ir_orig = new IR();
        Module mod =
            new Module("c:/foo.exe", 0xCAFE, 0xBEEF, Module.FileFormat.ELF,
                       Module.ISA.X64, "myModule");
        ir_orig.addModule(mod);

        UUID referentUuid = UUID.randomUUID();
        long symbolValue = 0x5A3C;
        // Symbol with no payload
        Symbol symbol0 = new Symbol("symbol0");
        // Symbol with value payload
        Symbol symbol1 = new Symbol("symbol1", symbolValue);
        // Symbol with referent payload
        Symbol symbol2 = new Symbol("symbol2", referentUuid);

        mod.addSymbol(symbol2);
        mod.addSymbol(symbol1);
        mod.addSymbol(symbol0);

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

        List<Symbol> symbols = mod_reloaded.getSymbols();
        for (Symbol symbol : symbols) {
            assertEquals(symbol.getModule(), Optional.of(mod_reloaded));
            if (symbol.getName().equals("symbol0")) {
                assertEquals(symbol.getPayloadType(), Symbol.PayloadType.NONE);
                assertEquals(symbol.getValue(), OptionalLong.empty());
                assertEquals(symbol.getReferentUuid(), Optional.empty());
            } else if (symbol.getName().equals("symbol1")) {
                assertEquals(symbol.getPayloadType(), Symbol.PayloadType.VALUE);
                assertEquals(symbol.getValue(), OptionalLong.of(symbolValue));
                assertEquals(symbol.getReferentUuid(), Optional.empty());
            } else if (symbol.getName().equals("symbol2")) {
                assertEquals(symbol.getPayloadType(),
                             Symbol.PayloadType.REFERENT);
                assertEquals(symbol.getValue(), OptionalLong.empty());
                assertEquals(symbol.getReferentUuid(),
                             Optional.of(referentUuid));
            } else
                fail("Found an unrecognized symbol: " + symbol.getName());
        }
    }

    @Test
    void testSymbolSetAndGet() throws Exception {

        Symbol symbol = new Symbol("badSymbol");
        symbol.setAtEnd(true);
        symbol.setName("goodSymbol");

        UUID referentUuid = UUID.randomUUID();
        long symbolValue = 0x5A3C;

        symbol.setReferentUuid(referentUuid);
        assertEquals(symbol.getReferentUuid(), Optional.of(referentUuid));
        assertEquals(symbol.getPayloadType(), Symbol.PayloadType.REFERENT);

        symbol.setValue(symbolValue);
        assertEquals(symbol.getValue(), OptionalLong.of(symbolValue));
        assertEquals(symbol.getPayloadType(), Symbol.PayloadType.VALUE);

        assertEquals(symbol.getModule(), Optional.empty());
        assertEquals(symbol.isAtEnd(), true);
        assertEquals(symbol.getName(), "goodSymbol");
    }
}
