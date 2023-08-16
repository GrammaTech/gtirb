package tests;

import static org.junit.jupiter.api.Assertions.*;

import com.grammatech.gtirb.*;
import com.grammatech.gtirb.AuxSerialization.*;
import com.grammatech.gtirb.Module;
import java.io.*;
import java.util.*;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

public class TestAuxData {

    private static Stream<Arguments> argProviderForTestCodec() {

        ArrayList<Integer> ali = new ArrayList<>();
        ali.add(4);
        ali.add(2);

        ArrayList<Float> alf = new ArrayList<>();
        alf.add(5.3f);
        HashMap<UUID, ArrayList<Float>> hm = new HashMap<>();
        hm.put(new UUID(2, 4), alf);

        HashSet<String> hs = new HashSet<>();
        hs.add("foo");
        hs.add("bar");

        return Stream.of(
            Arguments.of("bool", new BoolCodec(), false),
            Arguments.of("bool", new BoolCodec(), true),
            Arguments.of("float", new FloatCodec(), 0.0f),
            Arguments.of("float", new FloatCodec(), 42.5f),
            Arguments.of("int32_t", new IntegerCodec(), 0),
            Arguments.of("int32_t", new IntegerCodec(), 42),
            Arguments.of("int64_t", new LongCodec(), 0L),
            Arguments.of("int64_t", new LongCodec(), 42000000000L),
            Arguments.of("string", new StringCodec(), ""),
            Arguments.of("string", new StringCodec(), "abcdefg"),
            Arguments.of("UUID", new UuidCodec(), new UUID(0, 0)),
            Arguments.of("UUID", new UuidCodec(),
                         new UUID(0xFEEDFACECAFEBEEFL, 0xDEADFA11DEADD00DL)),
            Arguments.of("Offset", new OffsetCodec(),
                         new Offset(new UUID(4, 2), 1234)),
            Arguments.of("sequence<int32_t>",
                         new ArrayListCodec<>(new IntegerCodec()), ali),
            Arguments.of(
                "map<UUID,sequence<float>>",
                new HashMapCodec<>(new UuidCodec(),
                                   new ArrayListCodec<>(new FloatCodec())),
                hm),
            Arguments.of("set<string>", new HashSetCodec<>(new StringCodec()),
                         hs));
    }

    @ParameterizedTest(name = "Test codec for protobuf type: {0}")
    @MethodSource("argProviderForTestCodec")
    public <T> void testCodec(String typeName, Codec<T> codec, T val)
        throws IOException {
        assertEquals(codec.getTypeName(), typeName);
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        codec.encode(os, val);
        ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());
        T result = codec.decode(is);
        assertEquals(val, result);
    }

    @Test
    public void testAuxDataDecodeEncode() throws Exception {
        // Construct an IR+Module to dangle AuxData off of
        IR ir1 = new IR();
        Module m1 = new Module(
            "test", 0xDEADBEEF, 0, Module.FileFormat.ELF, Module.ISA.IA32,
            "test", new ArrayList<Section>(), new ArrayList<Symbol>(),
            new ArrayList<ProxyBlock>(), null);
        ir1.addModule(m1);

        // Add the AuxData
        assertEquals(Optional.empty(),
                     m1.getAuxData(AuxDataSchemas.functionNames));
        HashMap<UUID, UUID> func_names = new HashMap<>();
        func_names.put(new UUID(1, 2), new UUID(3, 4));
        m1.putAuxData(AuxDataSchemas.functionNames, func_names);
        Optional<HashMap<UUID, UUID>> oad1 =
            m1.getAuxData(AuxDataSchemas.functionNames);
        assertTrue(oad1.isPresent());
        assertEquals(func_names, oad1.get());

        // Serialize/unserialize
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        ir1.saveFile(os);

        ByteArrayInputStream is = new ByteArrayInputStream(os.toByteArray());
        IR ir2 = IR.loadFile(is);
        assertNotNull(ir2);
        List<Module> mods2 = ir2.getModules();
        assertNotNull(mods2);
        Module m2 = mods2.get(0);

        // Fetch the AuxData back.
        Optional<HashMap<UUID, UUID>> oad2 =
            m1.getAuxData(AuxDataSchemas.functionNames);
        assertTrue(oad2.isPresent());
        assertEquals(func_names, oad2.get());
    }
}
