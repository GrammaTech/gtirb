package tests;
import static org.junit.jupiter.api.Assertions.*;

import com.grammatech.gtirb.*;
import com.grammatech.gtirb.Module;
import com.grammatech.gtirb.Module.FileFormat;
import com.grammatech.gtirb.Module.ISA;
import java.io.File;
import java.util.*;
import org.junit.jupiter.api.Test;

public class TestByteIntervals {

    @Test
    void testByteIntervalSaveAndLoad() throws Exception {
        IR ir = new IR();
        Module module = new Module("c:/foo.exe", 0xCAFE, 0xBEEF, FileFormat.ELF,
                                   ISA.X64, "myModule");
        Section section =
            new Section("mySection", new HashSet<Section.SectionFlag>(),
                        new ArrayList<ByteInterval>());
        ByteInterval byteInterval =
            new ByteInterval("SaveAndLoad".getBytes(), 0xFEED);
        section.addByteInterval(byteInterval);
        ir.addModule(module);
        module.addSection(section);

        // Reset address
        byteInterval.clearAddress();

        File file = File.createTempFile("temp", null);
        String filename = file.getName();
        try {
            ir.saveFile(filename);
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

        ByteInterval biReloaded = ir_reloaded.getModules()
                                      .get(0)
                                      .getSections()
                                      .get(0)
                                      .getByteIntervals()
                                      .get(0);
        // Verify address was reset
        assertFalse(biReloaded.hasAddress());
        // Test set and get of address
        biReloaded.setAddress(0xDEADBEEF);
        assertEquals(biReloaded.getAddress(), OptionalLong.of(0xDEADBEEF));
        // Verify byte array content
        assertTrue(
            Arrays.equals(biReloaded.getBytes(), "SaveAndLoad".getBytes()));
    }

    @Test
    void testByteIntervalTruncation() throws Exception {
        ByteInterval bi = new ByteInterval();
        int bytesSize = 1000;
        byte[] bytes = new byte[bytesSize];

        bi.setSize(2000);
        bi.setBytes(bytes);
        assertEquals(bi.getSize(), 2000);
        assertEquals(bi.getInitializedSize(), bytesSize);
        bi.setSize(500);
        assertEquals(bi.getSize(), 500);
        assertEquals(bi.getInitializedSize(), 500);
        assertEquals(bi.getBytes().length, 500);
    }

    // Run through all blocks with a ByteBlock iterator
    @Test
    public void testIterators() {
        ByteInterval bi = new ByteInterval();

        CodeBlock b1 = new CodeBlock(4, 1, CodeBlock.DecodeMode.Default);
        CodeBlock b2 = new CodeBlock(3, 3, CodeBlock.DecodeMode.Default);
        CodeBlock b3 = new CodeBlock(5, 5, CodeBlock.DecodeMode.Thumb);
        DataBlock b4 = new DataBlock(3, 2);
        DataBlock b5 = new DataBlock(3, 6);

        bi.insertByteBlock(b1);
        bi.insertByteBlock(b2);
        bi.insertByteBlock(b3);
        bi.insertByteBlock(b4);
        bi.insertByteBlock(b5);

        List<ByteBlock> blockList = bi.getBlockList();
        assertEquals(5, blockList.size());

        Iterator<ByteBlock> blocks = bi.byteBlockIterator();
        // Dump of block types
        int codeblocks = 0;
        int datablocks = 0;

        while (blocks.hasNext()) {
            ByteBlock block = blocks.next();
            // String blockType;
            if (block instanceof CodeBlock)
                codeblocks += 1;
            //	blockType = "CODE";
            else
                datablocks += 1;
            //	blockType = "DATA";
        }

        assertEquals(3, codeblocks);
        assertEquals(2, datablocks);
    }

    // Try a few ByteBlock retrieval methods
    @Test
    public void testBlockRetrieval() {
        ByteInterval bi = new ByteInterval();
        bi.setAddress(0x200D90);
        bi.setSize(14);

        CodeBlock c1 = new CodeBlock(2, 1, CodeBlock.DecodeMode.Default);
        CodeBlock c2 = new CodeBlock(2, 2, CodeBlock.DecodeMode.Default);
        CodeBlock c3 = new CodeBlock(6, 3, CodeBlock.DecodeMode.Thumb);
        CodeBlock c4 = new CodeBlock(1, 4, CodeBlock.DecodeMode.Default);
        CodeBlock c5 = new CodeBlock(3, 5, CodeBlock.DecodeMode.Default);

        DataBlock d1 = new DataBlock(5, 0);
        DataBlock d2 = new DataBlock(4, 3);
        DataBlock d3 = new DataBlock(10, 4);
        DataBlock d4 = new DataBlock(1, 6);
        DataBlock d5 = new DataBlock(1, 7);

        bi.insertByteBlock(c1);
        bi.insertByteBlock(c2);
        bi.insertByteBlock(c3);
        bi.insertByteBlock(c4);
        bi.insertByteBlock(c5);

        bi.insertByteBlock(d1);
        bi.insertByteBlock(d2);
        bi.insertByteBlock(d3);
        bi.insertByteBlock(d4);
        bi.insertByteBlock(d5);

        List<com.grammatech.gtirb.DataBlock> intersecting =
            bi.findDataBlocksOn(0x200D96);
        assertNotNull(intersecting);
        assertEquals(3, intersecting.size());
        boolean saw_d2 = false;
        boolean saw_d3 = false;
        boolean saw_d4 = false;
        for (DataBlock dataBlock : intersecting) {
            if (dataBlock == d2) {
                saw_d2 = true;
            }
            if (dataBlock == d3) {
                saw_d3 = true;
            }
            if (dataBlock == d4) {
                saw_d4 = true;
            }
        }
        assertTrue(saw_d2);
        assertTrue(saw_d3);
        assertTrue(saw_d4);

        List<com.grammatech.gtirb.CodeBlock> startingAt =
            bi.findCodeBlocksAt(0x200D93, 0x200D95);
        assertNotNull(startingAt);
        assertEquals(2, startingAt.size());
        boolean saw_c3 = false;
        boolean saw_c4 = false;
        for (CodeBlock codeBlock : startingAt) {
            if (codeBlock == c3) {
                saw_c3 = true;
            }
            if (codeBlock == c4) {
                saw_c4 = true;
            }
        }
        assertTrue(saw_c3);
        assertTrue(saw_c4);
    }
}
