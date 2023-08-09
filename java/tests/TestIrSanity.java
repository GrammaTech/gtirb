package tests;
import static org.junit.jupiter.api.Assertions.*;

import com.grammatech.gtirb.*;
import com.grammatech.gtirb.Module;
import java.io.*;
import java.nio.charset.Charset;
import java.util.*;
import org.junit.jupiter.api.Test;

public class TestIrSanity {

    @Test
    void testCreateSaveAndLoad() throws Exception {
        // Just create a simple IR w/ 1 module
        IR ir_orig = new IR();
        Module mod =
            new Module("c:/foo.exe", 0xCAFE, 0xBEEF, Module.FileFormat.PE,
                       Module.ISA.X64, "foo.exe");
        ir_orig.addModule(mod);
        mod.setByteOrder(Module.ByteOrder.LittleEndian);
        ir_orig.setCfg(new CFG(new ArrayList<Edge>(), new ArrayList<byte[]>()));

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
        Module mod_reloaded = ir_reloaded.getModules().next();
        assertEquals("foo.exe", mod_reloaded.getName());
    }

    // TODO: The next few tests here each test different ways the loadFile()
    // function can fail. Unfortunately, in its current form, it only ever
    // returns null in each case, so we can't really tell that we're getting
    // the failure we expect. This should be improved if we ever make the
    // error reporting richer in the Java API.

    @Test
    void testNonGtirbContents() throws Exception {
        // A file with non-GTIRB contents.
        byte contents[] = "JUNK".getBytes(Charset.forName("ASCII"));
        ByteArrayInputStream file_proxy = new ByteArrayInputStream(contents);
        IR ir;
        ir = IR.loadFile(file_proxy);

        // IR should be null here
        assertNull(ir);
    }

    @Test
    void testWrongVersion() throws Exception {
        // A GTIRB file w/ the wrong version.
        ByteArrayOutputStream content_builder = new ByteArrayOutputStream();
        content_builder.write("GTIRB".getBytes(Charset.forName("ASCII")));
        content_builder.write(0);
        content_builder.write(0);
        content_builder.write(255);

        byte contents[] = content_builder.toByteArray();
        ByteArrayInputStream file_proxy = new ByteArrayInputStream(contents);
        IR ir;
        ir = IR.loadFile(file_proxy);

        // IR should be null here
        assertNull(ir);
    }

    @Test
    void testCorruptedProtobuf() throws Exception {
        // A GTIRB file w/ the right version but bad protobuf.
        ByteArrayOutputStream content_builder = new ByteArrayOutputStream();
        content_builder.write("GTIRB".getBytes(Charset.forName("ASCII")));
        content_builder.write(0);
        content_builder.write(0);
        content_builder.write(Version.gtirbProtobufVersion);
        content_builder.write(255);

        byte contents[] = content_builder.toByteArray();
        ByteArrayInputStream file_proxy = new ByteArrayInputStream(contents);
        IR ir;
        ir = IR.loadFile(file_proxy);

        // IR should be null here
        assertNull(ir);
    }

    @Test
    void testAddAndRemoveModules() throws Exception {
        // Create a simple IR
        IR ir = new IR();
        Module mod0 = new Module("/usr/bin/mod0", 0x0000, 0x0FFF,
                                 Module.FileFormat.ELF, Module.ISA.X64, "mod0");
        ir.addModule(mod0);
        Module mod1 = new Module("/usr/bin/mod0", 0x1000, 0x1FFF,
                                 Module.FileFormat.ELF, Module.ISA.X64, "mod1");
        ir.addModule(mod1);
        Module mod2 = new Module("/usr/bin/mod2", 0x2000, 0x2FFF,
                                 Module.FileFormat.ELF, Module.ISA.X64, "mod2");
        ir.addModule(mod2);
        Iterator<Module> modules = ir.getModules();
        int moduleCount = 0;
        while (modules.hasNext()) {
            moduleCount += 1;
            modules.next();
        }
        assertEquals(moduleCount, 3);

        ir.removeModule(mod0);
        ir.removeModule(mod2);

        // Now the only module left should be "mod1"
        assertEquals("mod1", ir.getModules().next().getName());
        assertEquals(mod1.getIr(), ir);
        assertNull(mod0.getIr());
        assertNull(mod2.getIr());
    }
}
