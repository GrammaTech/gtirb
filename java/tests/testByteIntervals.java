/**
 * A sample test.
 *
 * <p>Open a gtirb file and read its contents using the GTIRB Java API.
 */
import com.grammatech.gtirb.ByteBlock;
import com.grammatech.gtirb.ByteInterval;
import com.grammatech.gtirb.IR;
import com.grammatech.gtirb.Section;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Iterator;
import java.util.List;

public class testByteIntervals {

    // Run through all blocks with a ByteBlock iterator
    public static void testIterators(ByteInterval byteInterval) {
        Iterator<ByteBlock> blocks = byteInterval.byteBlockIterator();
        // Dump of block types
        int codeblocks = 0;
        int datablocks = 0;

        while (blocks.hasNext()) {
            ByteBlock block = blocks.next();
            // String blockType;
            if (block instanceof com.grammatech.gtirb.CodeBlock)
                codeblocks += 1;
            //	blockType = "CODE";
            else
                datablocks += 1;
            //	blockType = "DATA";
        }
        System.out.println(" - " + codeblocks + " code blocks and " +
                           datablocks + " data blocks.");
    }

    // Try a few ByteBlock retrieval methods
    public static void testBlockRetrieval(ByteInterval byteInterval) {
        List<com.grammatech.gtirb.DataBlock> intersecting =
            byteInterval.findDataBlocksOn(0x200D96);
        if (intersecting != null) {
            for (com.grammatech.gtirb.DataBlock dataBlock : intersecting) {
                System.out.println(
                    " - Data Block @ Address: " +
                    String.format("0x%08X", dataBlock.getAddress().orElse(0)) +
                    ", Size: " + dataBlock.getSize());
            }
        }

        List<com.grammatech.gtirb.CodeBlock> startingAt =
            byteInterval.findCodeBlocksAt(0x900, 0x9FF);
        if (startingAt != null) {
            for (com.grammatech.gtirb.CodeBlock codeBlock : startingAt) {
                if (codeBlock == null) {
                    System.err.println(" - null block!");
                    continue;
                }
                System.out.println(
                    " - Code Block @ Address: " +
                    String.format("0x%08X", codeBlock.getAddress().orElse(0)) +
                    ", Size: " + codeBlock.getSize());
            }
        }
    }

    // Drive the above tests by iterating through all the byte intervals in this
    // IR
    public static void testEachByteInterval(IR ir) {
        // Module has name collision with java.util
        List<com.grammatech.gtirb.Module> modules = ir.getModules();
        com.grammatech.gtirb.Module module = modules.get(0);

        List<Section> sections = module.getSections();
        for (Section section : sections) {
            List<ByteInterval> byteIntervals = section.getByteIntervals();
            for (ByteInterval byteInterval : byteIntervals) {
                System.out.println(
                    " Byte interval size: " + byteInterval.getSize() +
                    " address: " +
                    String.format("0x%08X",
                                  byteInterval.getAddress().orElse(0)));
                testBlockRetrieval(byteInterval);
                testIterators(byteInterval);
            }
        }
    }

    public static void main(String[] args) {

        if (args.length < 1) {
            System.err.println("No GTIRB file specified.");
            System.err.println("test failed.");
            return;
        }

        InputStream inputStream;
        boolean loadReturned = false;
        IR ir = null;
        String fileName = args[0];
        File inputFile = new File(fileName);
        try {
            inputStream = new FileInputStream(inputFile);
            ir = IR.loadFile(inputStream);
            if (ir == null) {
                loadReturned = false;
            } else {
                loadReturned = true;
            }
            inputStream.close();
        } catch (Exception e) {
            System.out.println("Unable to parse " + fileName + "." + e);
            System.err.println("test failed.");
            System.exit(1);
        }

        if (loadReturned != true) {
            System.out.println("Unable to load " + fileName + ".");
            System.err.println("test failed.");
            System.exit(1);
        }

        testEachByteInterval(ir);

        System.err.println("Byte Interval Test OK.");
        System.exit(0);
    }
}
