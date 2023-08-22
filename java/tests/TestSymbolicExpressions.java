package tests;

import static org.junit.jupiter.api.Assertions.*;

import com.grammatech.gtirb.*;
import com.grammatech.gtirb.Module;
import java.io.*;
import java.util.*;
import org.junit.jupiter.api.Test;

class TestSymbolicExpressions {

    // Test forward compatibility for unknown symbolic expression attributes.
    @Test
    void testUnknownAttributes() throws Exception {
        // Build minimal IR.

        IR ir = new IR();
        Module module =
            new Module("", 0, 0, Module.FileFormat.ELF, Module.ISA.X64, "test");
        Section section = new Section("foo", new HashSet<Section.SectionFlag>(),
                                      new ArrayList<ByteInterval>());
        Symbol symbol = new Symbol("bar", 0);
        ByteInterval byteInterval = new ByteInterval();
        SymbolicExpression expr =
            new SymAddrConst(0, symbol.getUuid(),
                             new HashSet<SymbolicExpression.AttributeFlag>());
        expr.addAttributeFlag(SymbolicExpression.AttributeFlag.GOT);
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

        assertTrue(expr.getAttributeFlags().contains(
            SymbolicExpression.AttributeFlag.GOT));
        assertTrue(expr.getUnknownAttributeFlags().contains(0xBEEF));
    }
}
