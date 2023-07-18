import com.grammatech.gtirb.ByteBlock;
import com.grammatech.gtirb.ByteInterval;
import com.grammatech.gtirb.IR;
import com.grammatech.gtirb.Module;
import com.grammatech.gtirb.Section;
import com.grammatech.gtirb.SymAddrConst;
import com.grammatech.gtirb.Symbol;
import com.grammatech.gtirb.SymbolicExpression;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.UUID;

public class testSymbolicExpressions {
    // Test forward compatibility for unknown symbolic expression attributes.
    public static void testUnknownAttributes() throws Exception {
        // Build minimal IR.
        ArrayList<Module> modules = new ArrayList();
        ArrayList<Section> sections = new ArrayList();
        ArrayList<ByteInterval> byteIntervals = new ArrayList();

        IR ir = new IR();
        Module module =
            new Module("", 0, 0, Module.FileFormat.ELF, Module.ISA.X64, "test");
        Section section =
            new Section("foo", new ArrayList(), new ArrayList(), module);
        Symbol symbol = new Symbol("bar", module);
        ByteInterval byteInterval = new ByteInterval(section);
        SymbolicExpression expr =
            new SymAddrConst(0, symbol.getUuid(), new ArrayList());
        expr.getAttributeFlags().add(SymbolicExpression.AttributeFlag.GOT);
        expr.getUnknownAttributeFlags().add(0xBEEF);
        byteInterval.insertSymbolicExpression(0, expr);

        modules.add(module);
        sections.add(section);
        byteIntervals.add(byteInterval);
        section.setByteIntervals(byteIntervals);
        module.setSections(sections);
        ir.setModules(modules);

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
        expr = byteInterval.symbolicExpressionIterator().next();

        assert expr.getAttributeFlags().contains(
            SymbolicExpression.AttributeFlag.GOT);
        assert expr.getUnknownAttributeFlags().contains(0xBEEF);
    }

    public static void main(String[] args) {
        try {
            testUnknownAttributes();
        } catch (Exception e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }

        System.err.println("Symbolic Expression Test OK.");
        System.exit(0);
    }
}
