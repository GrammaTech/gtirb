/**
 * A sample test.
 *
 * <p>Open a gtirb file and read its contents using the GTIRB Java API.
 */

import com.grammatech.gtirb.*;
import com.grammatech.gtirb.Module;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

public class testIrSanity {

    static boolean testLoadExistingIr(String fileName) {
        InputStream inputStream;
        boolean loadReturned = false;

        File inputFile = new File(fileName);
        try {
            inputStream = new FileInputStream(inputFile);
            IR ir = IR.loadFile(inputStream);
            if (ir == null) {
                loadReturned = false;
            } else {
                loadReturned = true;
            }
            inputStream.close();
        } catch (Exception e) {
            System.out.println("Unable to parse " + fileName + "." + e);
            return false;
        }

        if (loadReturned != true) {
            System.out.println("Unable to load " + fileName + ".");
            return false;
        }

        return true;
    }

    static boolean testCreateSaveAndLoad() {
        // Just create a simple IR w/ 1 module
        IR ir_orig = new IR();
        Module mod = new Module(
            "c:/foo.exe", 0xCAFE, 0xBEEF, Module.FileFormat.PE, Module.ISA.X64,
            "foo.exe", new ArrayList<Section>(), new ArrayList<Symbol>(),
            new ArrayList<ProxyBlock>(), null, ir_orig);
        mod.setByteOrder(Module.ByteOrder.LittleEndian);
        List<Module> mod_list = new ArrayList<Module>();
        mod_list.add(mod);
        ir_orig.setModules(mod_list);
        ir_orig.setCfg(new CFG(new ArrayList<Edge>(), new ArrayList<byte[]>()));

        File file;
        try {
            file = File.createTempFile("temp", null);
        } catch (Exception e) {
            System.out.println("Unable to create temp file for IR");
            return false;
        }

        String filename = file.getName();
        try {
            ir_orig.saveFile(filename);
        } catch (Exception e) {
            System.out.println("Unable to save test IR: " + e.getMessage());
            file.delete();
            return false;
        }

        IR ir_reloaded;
        try {
            ir_reloaded = IR.loadFile(filename);
        } catch (Exception e) {
            System.out.println("Unable to reload IR: " + e.getMessage());
            file.delete();
            return false;
        }

        file.delete();

        if (ir_reloaded == null) {
            System.out.println("Failed to parse reloaded IR");
            return false;
        }

        if (ir_reloaded.getModules().size() != 1) {
            System.out.println("Reloaded IR contents don't match");
            return false;
        }
        Module mod_reloaded = ir_reloaded.getModules().get(0);
        if (!mod_reloaded.getName().equals("foo.exe")) {
            System.out.println("Reloaded Module contents don't match");
            return false;
        }

        return true;
    }

    static boolean testInvalidContents() {
        // TODO: The tests here each test different ways the loadFile() function
        // can fail. Unfortunately, in its current form, it only ever returns
        // null in each case, so we can't really tell that we're getting the
        // failure we expect. This should be improved if we ever make the error
        // reporting richer in the Java API.

        // A file with non-GTIRB contents.
        {
            byte contents[] = "JUNK".getBytes(Charset.forName("ASCII"));
            ByteArrayInputStream file_proxy =
                new ByteArrayInputStream(contents);
            IR ir;
            try {
                ir = IR.loadFile(file_proxy);
            } catch (Exception e) {
                System.out.println("Exception thrown when it shouldn't have");
                return false;
            }
            // IR should be null here
            if (ir != null) {
                System.out.println(
                    "IR not null as expected when GTIRB magic is missing!");
                return false;
            }
        }

        // A GTIRB file w/ the wrong version.
        {
            ByteArrayOutputStream content_builder = new ByteArrayOutputStream();
            try {
                content_builder.write(
                    "GTIRB".getBytes(Charset.forName("ASCII")));
                content_builder.write(0);
                content_builder.write(0);
                content_builder.write(255);
            } catch (Exception e) {
                System.out.println("Exception thrown when it shouldn't have");
                return false;
            }
            byte contents[] = content_builder.toByteArray();
            ByteArrayInputStream file_proxy =
                new ByteArrayInputStream(contents);
            IR ir;
            try {
                ir = IR.loadFile(file_proxy);
            } catch (Exception e) {
                System.out.println("Exception thrown when it shouldn't have");
                return false;
            }
            // IR should be null here
            if (ir != null) {
                System.out.println(
                    "IR not null as expected when version number is wrong!");
                return false;
            }
        }

        // A GTIRB file w/ the right version but bad protobuf.
        {
            ByteArrayOutputStream content_builder = new ByteArrayOutputStream();
            try {
                content_builder.write(
                    "GTIRB".getBytes(Charset.forName("ASCII")));
                content_builder.write(0);
                content_builder.write(0);
                content_builder.write(Version.gtirbProtobufVersion);
                content_builder.write(255);
            } catch (Exception e) {
                System.out.println("Exception thrown when it shouldn't have");
                return false;
            }
            byte contents[] = content_builder.toByteArray();
            ByteArrayInputStream file_proxy =
                new ByteArrayInputStream(contents);
            IR ir;
            try {
                ir = IR.loadFile(file_proxy);
            } catch (Exception e) {
                System.out.println("Exception thrown when it shouldn't have");
                return false;
            }
            // IR should be null here
            if (ir != null) {
                System.out.println(
                    "IR not null as expected when protobuf is invalid!");
                return false;
            }
        }

        return true;
    }

    public static void main(String[] args) {

        if (args.length < 1) {
            System.err.println("No GTIRB file specified.");
            System.err.println("test failed.");
            return;
        }

        String fileName = args[0];
        boolean testOk = true;

        testOk &= testLoadExistingIr(fileName);
        testOk &= testCreateSaveAndLoad();
        testOk &= testInvalidContents();

        System.out.println("Version: " + Version.gtirbApiVersion);
        System.out.println("Protobuf Version: " + Version.gtirbProtobufVersion);

        if (testOk) {
            System.err.println("Sanity test OK.");
            System.exit(0);
        } else {
            System.err.println("test failed.");
            System.exit(1);
        }
    }
}
