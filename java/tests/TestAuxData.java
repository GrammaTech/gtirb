package tests;

import static org.junit.jupiter.api.Assertions.*;

import com.grammatech.gtirb.*;
import com.grammatech.gtirb.AuxSerialization.AuxDataSerialization;
import com.grammatech.gtirb.Module;
import java.io.*;
import java.util.*;
import org.junit.jupiter.api.Test;

public class TestAuxData {

    private static void serializeSelf(String type, Object value) {
        byte[] raw = AuxDataSerialization.encode(value, type);
        Object next = AuxDataSerialization.decode(raw, type);

        assertEquals(value, next);
    }

    @Test
    public void testSerialization() {
        serializeSelf("int32_t", Integer.valueOf(42));

        // FIXME: Something going wrong with these two:
        // serializeSelf("float", Float.valueOf(0.4f));
        // serializeSelf("double", Double.valueOf(1.0));

        serializeSelf("bool", Boolean.valueOf(true));
    }

    @Test
    public void testAuxDataDecodeEncode() throws Exception {
        IR ir1 = new IR();
        Module m1 = new Module(
            "test", 0xDEADBEEF, 0, Module.FileFormat.ELF, Module.ISA.IA32,
            "test", new ArrayList<Section>(), new ArrayList<Symbol>(),
            new ArrayList<ProxyBlock>(), null);
        ir1.addModule(m1);

        TreeMap<UUID, Long> sccMap1 = new TreeMap<>();
        UUID uuid = UUID.randomUUID();
        sccMap1.put(uuid, 42l);
        m1.setSccs(sccMap1);
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        ir1.saveFile(os);

        ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());
        IR ir2 = IR.loadFile(is);
        assertNotNull(ir2);
        Module m2 = ir2.getModules().next();
        Map<UUID, Long> sccMap2 = m2.getSccs();
        assertNotNull(sccMap2);
        Long v = sccMap2.get(uuid);
        assertNotNull(v);
        assertEquals(42l, v);
    }
}
