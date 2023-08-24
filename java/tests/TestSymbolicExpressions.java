package tests;

import static org.junit.jupiter.api.Assertions.*;

import com.grammatech.gtirb.*;
import com.grammatech.gtirb.CodeBlock.DecodeMode;
import com.grammatech.gtirb.Module;
import com.grammatech.gtirb.Module.FileFormat;
import com.grammatech.gtirb.Module.ISA;
import com.grammatech.gtirb.SymbolicExpression.AttributeFlag;
import java.io.*;
import java.util.*;
import org.junit.jupiter.api.Test;

class TestSymbolicExpressions {

    // Test forward compatibility for unknown symbolic expression attributes.
    @Test
    void testUnknownAttributes() throws Exception {
        // Build minimal IR.

        IR ir = new IR();
        Module module = new Module("", 0, 0, FileFormat.ELF, ISA.X64, "test");
        Section section = new Section("foo", new HashSet<Section.SectionFlag>(),
                                      new ArrayList<ByteInterval>());
        Symbol symbol = new Symbol("bar", 0);
        ByteInterval byteInterval = new ByteInterval();
        SymbolicExpression expr =
            new SymAddrConst(0, symbol.getUuid(), new HashSet<AttributeFlag>());
        expr.addAttributeFlag(AttributeFlag.GOT);
        expr.addUnknownFlag(0xBEEF);
        byteInterval.insertSymbolicExpression(0, expr);

        // byteIntervals.add(byteInterval);
        section.addByteInterval(byteInterval);
        module.addSection(section);
        ir.addModule(module);

        // Write IR to file.
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        ir.saveFile(output);

        // Load IR from file.
        ByteArrayInputStream input =
            new ByteArrayInputStream(output.toByteArray());
        ir = IR.loadFile(input);

        module = ir.getModules().get(0);
        section = module.getSections().get(0);
        byteInterval = section.getByteIntervals().get(0);
        expr = (SymbolicExpression)byteInterval.symbolicExpressionIterator()
                   .next()
                   .getValue();

        assertTrue(expr.getAttributeFlags().contains(AttributeFlag.GOT));
        assertTrue(expr.getUnknownAttributeFlags().contains(0xBEEF));
    }

    @Test
    void testSexprSetAndGet() throws Exception {
        Set<AttributeFlag> flags = new HashSet<AttributeFlag>();
        flags.add(AttributeFlag.PLT);
        CodeBlock codeBlockA = new CodeBlock(0x10L, 0x400L, DecodeMode.Default);
        CodeBlock codeBlockB = new CodeBlock(0x10L, 0x500L, DecodeMode.Default);

        // test set/get of decode mode
        codeBlockB.setDecodeMode(DecodeMode.Thumb);
        assertEquals(codeBlockB.getDecodeMode(), DecodeMode.Thumb);

        // test SymAddrAddr constructor
        Symbol symbolA = new Symbol("CodeBlockA", codeBlockA.getUuid());
        Symbol symbolB = new Symbol("CodeBlockB", codeBlockB.getUuid());
        SymbolicExpression expr = new SymAddrAddr(0x100L, 1L, symbolA.getUuid(),
                                                  symbolB.getUuid(), flags);

        // test adding and removing attribute flags
        expr.addAttributeFlag(AttributeFlag.S);
        assertEquals(expr.getAttributeFlags().size(), 2);
        expr.addAttributeFlag(AttributeFlag.PLT);
        assertEquals(expr.getAttributeFlags().size(), 2);
        expr.removeAttributeFlag(AttributeFlag.S);
        assertEquals(expr.getAttributeFlags().size(), 1);
        expr.removeUnknownFlag(0xF00eee);
        assertEquals(expr.getUnknownAttributeFlags().size(), 0);
    }
}
