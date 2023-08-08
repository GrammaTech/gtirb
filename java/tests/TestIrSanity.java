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
            ir_orig.addModule("c:/foo.exe", 0xCAFE, 0xBEEF,
                              Module.FileFormat.PE, Module.ISA.X64, "foo.exe");
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
}
