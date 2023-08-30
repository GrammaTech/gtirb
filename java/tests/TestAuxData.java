package tests;

import static org.junit.jupiter.api.Assertions.*;

import com.grammatech.gtirb.*;
import com.grammatech.gtirb.Module;
import com.grammatech.gtirb.auxdatacodec.*;
import com.grammatech.gtirb.auxdatacodec.Variant11Codec;
import com.grammatech.gtirb.auxdatacodec.Variant2Codec;
import com.grammatech.gtirb.auxdatacodec.Variant3Codec;
import com.grammatech.gtirb.tuple.*;
import com.grammatech.gtirb.variant.Token;
import com.grammatech.gtirb.variant.Variant11;
import com.grammatech.gtirb.variant.Variant2;
import com.grammatech.gtirb.variant.Variant3;
import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class FooSingle extends Tuple1<String> {
    public FooSingle(String s) { super(s); }
}

class FooPair extends Tuple2<String, Long> {
    public FooPair(String s, Long l) { super(s, l); }
}

class FooTriple extends Tuple3<String, Long, Float> {
    public FooTriple(String s, Long l, Float f) { super(s, l, f); }
}

class FooQuadruple extends Tuple4<String, Long, Float, Boolean> {
    public FooQuadruple(String s, Long l, Float f, Boolean b) {
        super(s, l, f, b);
    }
}

class FooQuintuple extends Tuple5<String, Long, Float, Boolean, Integer> {
    public FooQuintuple(String s, Long l, Float f, Boolean b, Integer i) {
        super(s, l, f, b, i);
    }
}

class FooVariant2 extends Variant2<Long, Float> {
    private FooVariant2(Token.T0 tok, Long l) { super(tok, l); }
    private FooVariant2(Token.T1 tok, Float f) { super(tok, f); }
    public static FooVariant2 ofLong(Long l) {
        return new FooVariant2(new Token.T0(), l);
    }
    public static FooVariant2 ofFloat(Float f) {
        return new FooVariant2(new Token.T1(), f);
    }
}

class FooVariant3 extends Variant3<Long, Float, Boolean> {
    private FooVariant3(Token.T0 tok, Long l) { super(tok, l); }
    private FooVariant3(Token.T1 tok, Float f) { super(tok, f); }
    private FooVariant3(Token.T2 tok, Boolean b) { super(tok, b); }
    public static FooVariant3 ofLong(Long l) {
        return new FooVariant3(new Token.T0(), l);
    }
    public static FooVariant3 ofFloat(Float f) {
        return new FooVariant3(new Token.T1(), f);
    }
    public static FooVariant3 ofBoolean(Boolean b) {
        return new FooVariant3(new Token.T2(), b);
    }
}

class FooVariant11
    extends Variant11<Long, Float, Boolean, Integer, String, Long, Float,
                      Boolean, Integer, String, Offset> {
    private FooVariant11(Token.T0 tok, Long l) { super(tok, l); }
    private FooVariant11(Token.T1 tok, Float f) { super(tok, f); }
    private FooVariant11(Token.T2 tok, Boolean b) { super(tok, b); }
    private FooVariant11(Token.T3 tok, Integer i) { super(tok, i); }
    private FooVariant11(Token.T4 tok, String s) { super(tok, s); }
    private FooVariant11(Token.T5 tok, Long l) { super(tok, l); }
    private FooVariant11(Token.T6 tok, Float f) { super(tok, f); }
    private FooVariant11(Token.T7 tok, Boolean b) { super(tok, b); }
    private FooVariant11(Token.T8 tok, Integer i) { super(tok, i); }
    private FooVariant11(Token.T9 tok, String s) { super(tok, s); }
    private FooVariant11(Token.T10 tok, Offset o) { super(tok, o); }
    public static FooVariant11 ofLong1(Long l) {
        return new FooVariant11(new Token.T0(), l);
    }
    public static FooVariant11 ofFloat1(Float f) {
        return new FooVariant11(new Token.T1(), f);
    }
    public static FooVariant11 ofBoolean1(Boolean b) {
        return new FooVariant11(new Token.T2(), b);
    }
    public static FooVariant11 ofInteger1(Integer i) {
        return new FooVariant11(new Token.T3(), i);
    }
    public static FooVariant11 ofString1(String s) {
        return new FooVariant11(new Token.T4(), s);
    }
    public static FooVariant11 ofLong2(Long l) {
        return new FooVariant11(new Token.T5(), l);
    }
    public static FooVariant11 ofFloat2(Float f) {
        return new FooVariant11(new Token.T6(), f);
    }
    public static FooVariant11 ofBoolean2(Boolean b) {
        return new FooVariant11(new Token.T7(), b);
    }
    public static FooVariant11 ofInteger2(Integer i) {
        return new FooVariant11(new Token.T8(), i);
    }
    public static FooVariant11 ofString2(String s) {
        return new FooVariant11(new Token.T9(), s);
    }
    public static FooVariant11 ofOffset(Offset o) {
        return new FooVariant11(new Token.T10(), o);
    }
}

public class TestAuxData {

    private static Stream<Arguments> argProviderForTestCodec() {

        List<Integer> ali = new ArrayList<>();
        ali.add(4);
        ali.add(2);

        List<Float> alf = new ArrayList<>();
        alf.add(5.3f);
        Map<UUID, List<Float>> hm = new HashMap<>();
        hm.put(new UUID(2, 4), alf);

        Set<String> hs = new HashSet<>();
        hs.add("foo");
        hs.add("bar");

        Variant11Codec<FooVariant11, Long, Float, Boolean, Integer, String,
                       Long, Float, Boolean, Integer, String, Offset>
            fooV11Codec = new Variant11Codec<>(
                LongCodec.INT64, new FloatCodec(), new BoolCodec(),
                IntegerCodec.INT32, new StringCodec(), LongCodec.INT64,
                new FloatCodec(), new BoolCodec(), IntegerCodec.INT32,
                new StringCodec(), new OffsetCodec(), FooVariant11::ofLong1,
                FooVariant11::ofFloat1, FooVariant11::ofBoolean1,
                FooVariant11::ofInteger1, FooVariant11::ofString1,
                FooVariant11::ofLong2, FooVariant11::ofFloat2,
                FooVariant11::ofBoolean2, FooVariant11::ofInteger2,
                FooVariant11::ofString2, FooVariant11::ofOffset);

        String expFooV11Name =
            "variant<int64_t,float,bool,int32_t,string,int64_t,float,bool,int32_t,string,Offset>";

        return Stream.of(
            Arguments.of("bool", new BoolCodec(), false),
            Arguments.of("bool", new BoolCodec(), true),
            Arguments.of("int8_t", ByteCodec.INT8, (byte)0),
            Arguments.of("int8_t", ByteCodec.INT8, (byte)42),
            Arguments.of("uint8_t", ByteCodec.UINT8, (byte)43),
            Arguments.of("float", new FloatCodec(), 0.0f),
            Arguments.of("float", new FloatCodec(), 42.5f),
            Arguments.of("int16_t", ShortCodec.INT16, (short)0),
            Arguments.of("int16_t", ShortCodec.INT16, (short)42),
            Arguments.of("uint16_t", ShortCodec.UINT16, (short)43),
            Arguments.of("int32_t", IntegerCodec.INT32, 0),
            Arguments.of("int32_t", IntegerCodec.INT32, 42),
            Arguments.of("uint32_t", IntegerCodec.UINT32, 43),
            Arguments.of("int64_t", LongCodec.INT64, 0L),
            Arguments.of("int64_t", LongCodec.INT64, 42000000000L),
            Arguments.of("uint64_t", LongCodec.UINT64, 43000000000L),
            Arguments.of("string", new StringCodec(), ""),
            Arguments.of("string", new StringCodec(), "abcdefg"),
            Arguments.of("UUID", new UuidCodec(), new UUID(0, 0)),
            Arguments.of("UUID", new UuidCodec(),
                         new UUID(0xFEEDFACECAFEBEEFL, 0xDEADFA11DEADD00DL)),
            Arguments.of("Offset", new OffsetCodec(),
                         new Offset(new UUID(4, 2), 1234)),
            Arguments.of("sequence<int32_t>",
                         new ListCodec<>(IntegerCodec.INT32, ArrayList::new),
                         ali),
            Arguments.of("map<UUID,sequence<float>>",
                         new MapCodec<>(
                             new UuidCodec(),
                             new ListCodec<>(new FloatCodec(), ArrayList::new),
                             HashMap::new),
                         hm),
            Arguments.of("set<string>",
                         new SetCodec<>(new StringCodec(), HashSet::new), hs),
            Arguments.of("tuple<string>",
                         new Tuple1Codec<>(new StringCodec(), FooSingle::new),
                         new FooSingle("hello")),
            Arguments.of("tuple<string,int64_t>",
                         new Tuple2Codec<>(new StringCodec(), LongCodec.INT64,
                                           FooPair::new),
                         new FooPair("hello", 27L)),
            Arguments.of("tuple<string,int64_t,float>",
                         new Tuple3Codec<>(new StringCodec(), LongCodec.INT64,
                                           new FloatCodec(), FooTriple::new),
                         new FooTriple("hello", 27L, 3.14f)),
            Arguments.of("tuple<string,int64_t,float,bool>",
                         new Tuple4Codec<>(new StringCodec(), LongCodec.INT64,
                                           new FloatCodec(), new BoolCodec(),
                                           FooQuadruple::new),
                         new FooQuadruple("hello", 27L, 3.14f, true)),
            Arguments.of("tuple<string,int64_t,float,bool,int32_t>",
                         new Tuple5Codec<>(new StringCodec(), LongCodec.INT64,
                                           new FloatCodec(), new BoolCodec(),
                                           IntegerCodec.INT32,
                                           FooQuintuple::new),
                         new FooQuintuple("hello", 27L, 3.14f, true, 42)),
            Arguments.of("variant<int64_t,float>",
                         new Variant2Codec<>(LongCodec.INT64, new FloatCodec(),
                                             FooVariant2::ofLong,
                                             FooVariant2::ofFloat),
                         FooVariant2.ofLong(42L)),
            Arguments.of("variant<int64_t,float>",
                         new Variant2Codec<>(LongCodec.INT64, new FloatCodec(),
                                             FooVariant2::ofLong,
                                             FooVariant2::ofFloat),
                         FooVariant2.ofFloat(3.14f)),
            Arguments.of("variant<int64_t,float,bool>",
                         new Variant3Codec<>(
                             LongCodec.INT64, new FloatCodec(), new BoolCodec(),
                             FooVariant3::ofLong, FooVariant3::ofFloat,
                             FooVariant3::ofBoolean),
                         FooVariant3.ofBoolean(true)),
            Arguments.of(expFooV11Name, fooV11Codec, FooVariant11.ofLong1(42L)),
            Arguments.of(expFooV11Name, fooV11Codec,
                         FooVariant11.ofFloat1(3.14f)),
            Arguments.of(expFooV11Name, fooV11Codec,
                         FooVariant11.ofBoolean1(true)),
            Arguments.of(expFooV11Name, fooV11Codec,
                         FooVariant11.ofInteger1(43)),
            Arguments.of(expFooV11Name, fooV11Codec,
                         FooVariant11.ofString1("abc")),
            Arguments.of(expFooV11Name, fooV11Codec, FooVariant11.ofLong2(44L)),
            Arguments.of(expFooV11Name, fooV11Codec,
                         FooVariant11.ofFloat2(3.145f)),
            Arguments.of(expFooV11Name, fooV11Codec,
                         FooVariant11.ofBoolean2(false)),
            Arguments.of(expFooV11Name, fooV11Codec,
                         FooVariant11.ofInteger2(45)),
            Arguments.of(expFooV11Name, fooV11Codec,
                         FooVariant11.ofString2("def")),
            Arguments.of(
                expFooV11Name, fooV11Codec,
                FooVariant11.ofOffset(new Offset(new UUID(9, 10), 11))));
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
    public void testAuxDataDecodeEncode() throws IOException {
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
        Map<UUID, UUID> func_names = new TreeMap<>();
        func_names.put(new UUID(1, 2), new UUID(3, 4));
        m1.putAuxData(AuxDataSchemas.functionNames, func_names);
        Optional<Map<UUID, UUID>> oad1 =
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
        Optional<Map<UUID, UUID>> oad2 =
            m1.getAuxData(AuxDataSchemas.functionNames);
        assertTrue(oad2.isPresent());
        assertEquals(func_names, oad2.get());
    }

    AuxDataSchema<Long> aSchema =
        new AuxDataSchema<>("aSchema", LongCodec.UINT64);
    AuxDataSchema<Boolean> anotherSchema =
        new AuxDataSchema<>("anotherSchema", new BoolCodec());

    @Test
    public void testRemoveAuxData() throws IOException {
        Module m = new Module("test", 0xDEADBEEF, 0, Module.FileFormat.ELF,
                              Module.ISA.IA32, "test", new ArrayList<Section>(),
                              new ArrayList<Symbol>(),
                              new ArrayList<ProxyBlock>(), null);

        m.putAuxData(aSchema, 42L);
        assertEquals(Optional.of(42L), m.getAuxData(aSchema));

        boolean rv = m.removeAuxData("aSchema");
        assertTrue(rv);
        assertEquals(Optional.empty(), m.getAuxData(aSchema));
        rv = m.removeAuxData("aSchema");
        assertFalse(rv);

        m.putAuxData(aSchema, 43L);
        assertEquals(Optional.of(43L), m.getAuxData(aSchema));

        rv = m.removeAuxData(aSchema);
        assertTrue(rv);
        assertEquals(Optional.empty(), m.getAuxData(aSchema));
        rv = m.removeAuxData(aSchema);
        assertFalse(rv);
    }

    @Test
    public void testClearAuxData() throws IOException {
        Module m = new Module("test", 0xDEADBEEF, 0, Module.FileFormat.ELF,
                              Module.ISA.IA32, "test", new ArrayList<Section>(),
                              new ArrayList<Symbol>(),
                              new ArrayList<ProxyBlock>(), null);

        m.putAuxData(aSchema, 42L);
        m.putAuxData(anotherSchema, true);
        assertEquals(Optional.of(42L), m.getAuxData(aSchema));
        assertEquals(Optional.of(true), m.getAuxData(anotherSchema));

        m.clearAuxData();
        assertEquals(Optional.empty(), m.getAuxData(aSchema));
        assertEquals(Optional.empty(), m.getAuxData(anotherSchema));
    }

    @Test
    public void testGetAuxDataMap() throws IOException {
        Module m = new Module("test", 0xDEADBEEF, 0, Module.FileFormat.ELF,
                              Module.ISA.IA32, "test", new ArrayList<Section>(),
                              new ArrayList<Symbol>(),
                              new ArrayList<ProxyBlock>(), null);

        m.putAuxData(aSchema, 42L);
        m.putAuxData(anotherSchema, true);
        assertEquals(Optional.of(42L), m.getAuxData(aSchema));
        assertEquals(Optional.of(true), m.getAuxData(anotherSchema));

        Map<String, AuxDataContainer.AuxData> adMap = m.getAuxDataMap();
        assertTrue(adMap.containsKey(aSchema.getName()));
        assertTrue(adMap.containsKey(anotherSchema.getName()));
        assertThrows(UnsupportedOperationException.class, () -> adMap.clear());
    }
}
