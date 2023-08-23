package tests;

import static org.junit.jupiter.api.Assertions.*;

import com.grammatech.gtirb.Offset;
import com.grammatech.gtirb.variant.Token;
import com.grammatech.gtirb.variant.Variant11;
import com.grammatech.gtirb.variant.Variant2;
import com.grammatech.gtirb.variant.Variant3;
import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.Test;

class FooVariant2 extends Variant2<Long, Float> {
    private FooVariant2(Token.T0 tok, Long l) { super(tok, l); }
    private FooVariant2(Token.T1 tok, Float f) { super(tok, f); }
    public Optional<Long> getLong() { return this.get0(); }
    public Optional<Float> getFloat() { return this.get1(); }
    public void setLong(Long l) { this.set0(l); }
    public void setFloat(Float f) { this.set1(f); }
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
    public Optional<Long> getLong() { return this.get0(); }
    public Optional<Float> getFloat() { return this.get1(); }
    public Optional<Boolean> getBoolean() { return this.get2(); }
    public void setLong(Long l) { this.set0(l); }
    public void setFloat(Float f) { this.set1(f); }
    public void setBoolean(Boolean b) { this.set2(b); }
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
    public Optional<Long> getLong1() { return this.get0(); }
    public Optional<Float> getFloat1() { return this.get1(); }
    public Optional<Boolean> getBoolean1() { return this.get2(); }
    public Optional<Integer> getInteger1() { return this.get3(); }
    public Optional<String> getString1() { return this.get4(); }
    public Optional<Long> getLong2() { return this.get5(); }
    public Optional<Float> getFloat2() { return this.get6(); }
    public Optional<Boolean> getBoolean2() { return this.get7(); }
    public Optional<Integer> getInteger2() { return this.get8(); }
    public Optional<String> getString2() { return this.get9(); }
    public Optional<Offset> getOffset() { return this.get10(); }
    public void setLong1(Long l) { this.set0(l); }
    public void setFloat1(Float f) { this.set1(f); }
    public void setBoolean1(Boolean b) { this.set2(b); }
    public void setInteger1(Integer i) { this.set3(i); }
    public void setString1(String s) { this.set4(s); }
    public void setLong2(Long l) { this.set5(l); }
    public void setFloat2(Float f) { this.set6(f); }
    public void setBoolean2(Boolean b) { this.set7(b); }
    public void setInteger2(Integer i) { this.set8(i); }
    public void setString2(String s) { this.set9(s); }
    public void setOffset(Offset o) { this.set10(o); }
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

public class TestVariant {
    @Test
    public void testVariant2() {
        FooVariant2 fv1 = FooVariant2.ofLong(42L);
        assertEquals(Optional.of(42L), fv1.getLong());
        assertEquals(Optional.empty(), fv1.getFloat());
        fv1.setLong(43L);
        assertEquals(Optional.of(43L), fv1.getLong());
        assertEquals(Optional.empty(), fv1.getFloat());
        fv1.setFloat(3.14f);
        assertEquals(Optional.empty(), fv1.getLong());
        assertEquals(Optional.of(3.14f), fv1.getFloat());

        FooVariant2 fv2 = FooVariant2.ofFloat(3.14f);
        assertEquals(Optional.empty(), fv2.getLong());
        assertEquals(Optional.of(3.14f), fv2.getFloat());
        assertEquals(fv1, fv2);
    }

    @Test
    public void testVariant3() {
        FooVariant3 fv1 = FooVariant3.ofLong(42L);
        assertEquals(Optional.of(42L), fv1.getLong());
        assertEquals(Optional.empty(), fv1.getFloat());
        assertEquals(Optional.empty(), fv1.getBoolean());
        fv1.setLong(43L);
        assertEquals(Optional.of(43L), fv1.getLong());
        assertEquals(Optional.empty(), fv1.getFloat());
        assertEquals(Optional.empty(), fv1.getBoolean());
        fv1.setBoolean(true);
        assertEquals(Optional.empty(), fv1.getLong());
        assertEquals(Optional.empty(), fv1.getFloat());
        assertEquals(Optional.of(true), fv1.getBoolean());
        fv1.setFloat(3.14f);
        assertEquals(Optional.empty(), fv1.getLong());
        assertEquals(Optional.of(3.14f), fv1.getFloat());
        assertEquals(Optional.empty(), fv1.getBoolean());

        FooVariant3 fv2 = FooVariant3.ofFloat(3.14f);
        assertEquals(Optional.empty(), fv2.getLong());
        assertEquals(Optional.of(3.14f), fv2.getFloat());
        assertEquals(Optional.empty(), fv2.getBoolean());
        assertEquals(fv1, fv2);

        FooVariant3 fv3 = FooVariant3.ofBoolean(false);
        assertEquals(Optional.empty(), fv3.getLong());
        assertEquals(Optional.empty(), fv3.getFloat());
        assertEquals(Optional.of(false), fv3.getBoolean());
    }

    @Test
    public void testVariant11() {
        FooVariant11 fv1 = FooVariant11.ofLong1(42L);
        assertEquals(Optional.of(42L), fv1.getLong1());
        assertEquals(Optional.empty(), fv1.getFloat1());
        assertEquals(Optional.empty(), fv1.getBoolean1());
        assertEquals(Optional.empty(), fv1.getInteger1());
        assertEquals(Optional.empty(), fv1.getString1());
        assertEquals(Optional.empty(), fv1.getLong2());
        assertEquals(Optional.empty(), fv1.getFloat2());
        assertEquals(Optional.empty(), fv1.getBoolean2());
        assertEquals(Optional.empty(), fv1.getInteger2());
        assertEquals(Optional.empty(), fv1.getString2());
        assertEquals(Optional.empty(), fv1.getOffset());
        fv1.setLong1(43L);
        assertEquals(Optional.of(43L), fv1.getLong1());
        assertEquals(Optional.empty(), fv1.getFloat1());
        assertEquals(Optional.empty(), fv1.getBoolean1());
        assertEquals(Optional.empty(), fv1.getInteger1());
        assertEquals(Optional.empty(), fv1.getString1());
        assertEquals(Optional.empty(), fv1.getLong2());
        assertEquals(Optional.empty(), fv1.getFloat2());
        assertEquals(Optional.empty(), fv1.getBoolean2());
        assertEquals(Optional.empty(), fv1.getInteger2());
        assertEquals(Optional.empty(), fv1.getString2());
        assertEquals(Optional.empty(), fv1.getOffset());
        fv1.setBoolean1(true);
        assertEquals(Optional.empty(), fv1.getLong1());
        assertEquals(Optional.empty(), fv1.getFloat1());
        assertEquals(Optional.of(true), fv1.getBoolean1());
        assertEquals(Optional.empty(), fv1.getInteger1());
        assertEquals(Optional.empty(), fv1.getString1());
        assertEquals(Optional.empty(), fv1.getLong2());
        assertEquals(Optional.empty(), fv1.getFloat2());
        assertEquals(Optional.empty(), fv1.getBoolean2());
        assertEquals(Optional.empty(), fv1.getInteger2());
        assertEquals(Optional.empty(), fv1.getString2());
        assertEquals(Optional.empty(), fv1.getOffset());
        fv1.setInteger1(44);
        assertEquals(Optional.empty(), fv1.getLong1());
        assertEquals(Optional.empty(), fv1.getFloat1());
        assertEquals(Optional.empty(), fv1.getBoolean1());
        assertEquals(Optional.of(44), fv1.getInteger1());
        assertEquals(Optional.empty(), fv1.getString1());
        assertEquals(Optional.empty(), fv1.getLong2());
        assertEquals(Optional.empty(), fv1.getFloat2());
        assertEquals(Optional.empty(), fv1.getBoolean2());
        assertEquals(Optional.empty(), fv1.getInteger2());
        assertEquals(Optional.empty(), fv1.getString2());
        assertEquals(Optional.empty(), fv1.getOffset());
        fv1.setString1("abc");
        assertEquals(Optional.empty(), fv1.getLong1());
        assertEquals(Optional.empty(), fv1.getFloat1());
        assertEquals(Optional.empty(), fv1.getBoolean1());
        assertEquals(Optional.empty(), fv1.getInteger1());
        assertEquals(Optional.of("abc"), fv1.getString1());
        assertEquals(Optional.empty(), fv1.getLong2());
        assertEquals(Optional.empty(), fv1.getFloat2());
        assertEquals(Optional.empty(), fv1.getBoolean2());
        assertEquals(Optional.empty(), fv1.getInteger2());
        assertEquals(Optional.empty(), fv1.getString2());
        assertEquals(Optional.empty(), fv1.getOffset());
        fv1.setLong2(45L);
        assertEquals(Optional.empty(), fv1.getLong1());
        assertEquals(Optional.empty(), fv1.getFloat1());
        assertEquals(Optional.empty(), fv1.getBoolean1());
        assertEquals(Optional.empty(), fv1.getInteger1());
        assertEquals(Optional.empty(), fv1.getString1());
        assertEquals(Optional.of(45L), fv1.getLong2());
        assertEquals(Optional.empty(), fv1.getFloat2());
        assertEquals(Optional.empty(), fv1.getBoolean2());
        assertEquals(Optional.empty(), fv1.getInteger2());
        assertEquals(Optional.empty(), fv1.getString2());
        assertEquals(Optional.empty(), fv1.getOffset());
        fv1.setFloat2(3.16f);
        assertEquals(Optional.empty(), fv1.getLong1());
        assertEquals(Optional.empty(), fv1.getFloat1());
        assertEquals(Optional.empty(), fv1.getBoolean1());
        assertEquals(Optional.empty(), fv1.getInteger1());
        assertEquals(Optional.empty(), fv1.getString1());
        assertEquals(Optional.empty(), fv1.getLong2());
        assertEquals(Optional.of(3.16f), fv1.getFloat2());
        assertEquals(Optional.empty(), fv1.getBoolean2());
        assertEquals(Optional.empty(), fv1.getInteger2());
        assertEquals(Optional.empty(), fv1.getString2());
        assertEquals(Optional.empty(), fv1.getOffset());
        fv1.setBoolean2(false);
        assertEquals(Optional.empty(), fv1.getLong1());
        assertEquals(Optional.empty(), fv1.getFloat1());
        assertEquals(Optional.empty(), fv1.getBoolean1());
        assertEquals(Optional.empty(), fv1.getInteger1());
        assertEquals(Optional.empty(), fv1.getString1());
        assertEquals(Optional.empty(), fv1.getLong2());
        assertEquals(Optional.empty(), fv1.getFloat2());
        assertEquals(Optional.of(false), fv1.getBoolean2());
        assertEquals(Optional.empty(), fv1.getInteger2());
        assertEquals(Optional.empty(), fv1.getString2());
        assertEquals(Optional.empty(), fv1.getOffset());
        fv1.setInteger2(46);
        assertEquals(Optional.empty(), fv1.getLong1());
        assertEquals(Optional.empty(), fv1.getFloat1());
        assertEquals(Optional.empty(), fv1.getBoolean1());
        assertEquals(Optional.empty(), fv1.getInteger1());
        assertEquals(Optional.empty(), fv1.getString1());
        assertEquals(Optional.empty(), fv1.getLong2());
        assertEquals(Optional.empty(), fv1.getFloat2());
        assertEquals(Optional.empty(), fv1.getBoolean2());
        assertEquals(Optional.of(46), fv1.getInteger2());
        assertEquals(Optional.empty(), fv1.getString2());
        assertEquals(Optional.empty(), fv1.getOffset());
        fv1.setString2("def");
        assertEquals(Optional.empty(), fv1.getLong1());
        assertEquals(Optional.empty(), fv1.getFloat1());
        assertEquals(Optional.empty(), fv1.getBoolean1());
        assertEquals(Optional.empty(), fv1.getInteger1());
        assertEquals(Optional.empty(), fv1.getString1());
        assertEquals(Optional.empty(), fv1.getLong2());
        assertEquals(Optional.empty(), fv1.getFloat2());
        assertEquals(Optional.empty(), fv1.getBoolean2());
        assertEquals(Optional.empty(), fv1.getInteger2());
        assertEquals(Optional.of("def"), fv1.getString2());
        assertEquals(Optional.empty(), fv1.getOffset());
        fv1.setOffset(new Offset(new UUID(7, 8), 9));
        assertEquals(Optional.empty(), fv1.getLong1());
        assertEquals(Optional.empty(), fv1.getFloat1());
        assertEquals(Optional.empty(), fv1.getBoolean1());
        assertEquals(Optional.empty(), fv1.getInteger1());
        assertEquals(Optional.empty(), fv1.getString1());
        assertEquals(Optional.empty(), fv1.getLong2());
        assertEquals(Optional.empty(), fv1.getFloat2());
        assertEquals(Optional.empty(), fv1.getBoolean2());
        assertEquals(Optional.empty(), fv1.getInteger2());
        assertEquals(Optional.empty(), fv1.getString2());
        assertEquals(Optional.of(new Offset(new UUID(7, 8), 9)),
                     fv1.getOffset());
        fv1.setFloat1(3.14f);
        assertEquals(Optional.empty(), fv1.getLong1());
        assertEquals(Optional.of(3.14f), fv1.getFloat1());
        assertEquals(Optional.empty(), fv1.getBoolean1());
        assertEquals(Optional.empty(), fv1.getInteger1());
        assertEquals(Optional.empty(), fv1.getString1());
        assertEquals(Optional.empty(), fv1.getLong2());
        assertEquals(Optional.empty(), fv1.getFloat2());
        assertEquals(Optional.empty(), fv1.getBoolean2());
        assertEquals(Optional.empty(), fv1.getInteger2());
        assertEquals(Optional.empty(), fv1.getString2());
        assertEquals(Optional.empty(), fv1.getOffset());

        FooVariant11 fv2 = FooVariant11.ofFloat1(3.14f);
        assertEquals(Optional.empty(), fv2.getLong1());
        assertEquals(Optional.of(3.14f), fv2.getFloat1());
        assertEquals(Optional.empty(), fv2.getBoolean1());
        assertEquals(Optional.empty(), fv2.getInteger1());
        assertEquals(Optional.empty(), fv2.getString1());
        assertEquals(Optional.empty(), fv2.getLong2());
        assertEquals(Optional.empty(), fv2.getFloat2());
        assertEquals(Optional.empty(), fv2.getBoolean2());
        assertEquals(Optional.empty(), fv2.getInteger2());
        assertEquals(Optional.empty(), fv2.getString2());
        assertEquals(Optional.empty(), fv2.getOffset());
        assertEquals(fv1, fv2);

        FooVariant11 fv3 = FooVariant11.ofBoolean1(false);
        assertEquals(Optional.empty(), fv3.getLong1());
        assertEquals(Optional.empty(), fv3.getFloat1());
        assertEquals(Optional.of(false), fv3.getBoolean1());
        assertEquals(Optional.empty(), fv3.getInteger1());
        assertEquals(Optional.empty(), fv3.getString1());
        assertEquals(Optional.empty(), fv3.getLong2());
        assertEquals(Optional.empty(), fv3.getFloat2());
        assertEquals(Optional.empty(), fv3.getBoolean2());
        assertEquals(Optional.empty(), fv3.getInteger2());
        assertEquals(Optional.empty(), fv3.getString2());
        assertEquals(Optional.empty(), fv3.getOffset());

        FooVariant11 fv4 = FooVariant11.ofInteger1(47);
        assertEquals(Optional.empty(), fv4.getLong1());
        assertEquals(Optional.empty(), fv4.getFloat1());
        assertEquals(Optional.empty(), fv4.getBoolean1());
        assertEquals(Optional.of(47), fv4.getInteger1());
        assertEquals(Optional.empty(), fv4.getString1());
        assertEquals(Optional.empty(), fv4.getLong2());
        assertEquals(Optional.empty(), fv4.getFloat2());
        assertEquals(Optional.empty(), fv4.getBoolean2());
        assertEquals(Optional.empty(), fv4.getInteger2());
        assertEquals(Optional.empty(), fv4.getString2());
        assertEquals(Optional.empty(), fv4.getOffset());

        FooVariant11 fv5 = FooVariant11.ofString1("ghi");
        assertEquals(Optional.empty(), fv5.getLong1());
        assertEquals(Optional.empty(), fv5.getFloat1());
        assertEquals(Optional.empty(), fv5.getBoolean1());
        assertEquals(Optional.empty(), fv5.getInteger1());
        assertEquals(Optional.of("ghi"), fv5.getString1());
        assertEquals(Optional.empty(), fv5.getLong2());
        assertEquals(Optional.empty(), fv5.getFloat2());
        assertEquals(Optional.empty(), fv5.getBoolean2());
        assertEquals(Optional.empty(), fv5.getInteger2());
        assertEquals(Optional.empty(), fv5.getString2());
        assertEquals(Optional.empty(), fv5.getOffset());

        FooVariant11 fv6 = FooVariant11.ofLong2(48L);
        assertEquals(Optional.empty(), fv6.getLong1());
        assertEquals(Optional.empty(), fv6.getFloat1());
        assertEquals(Optional.empty(), fv6.getBoolean1());
        assertEquals(Optional.empty(), fv6.getInteger1());
        assertEquals(Optional.empty(), fv6.getString1());
        assertEquals(Optional.of(48L), fv6.getLong2());
        assertEquals(Optional.empty(), fv6.getFloat2());
        assertEquals(Optional.empty(), fv6.getBoolean2());
        assertEquals(Optional.empty(), fv6.getInteger2());
        assertEquals(Optional.empty(), fv6.getString2());
        assertEquals(Optional.empty(), fv6.getOffset());

        FooVariant11 fv7 = FooVariant11.ofFloat2(3.17f);
        assertEquals(Optional.empty(), fv7.getLong1());
        assertEquals(Optional.empty(), fv7.getFloat1());
        assertEquals(Optional.empty(), fv7.getBoolean1());
        assertEquals(Optional.empty(), fv7.getInteger1());
        assertEquals(Optional.empty(), fv7.getString1());
        assertEquals(Optional.empty(), fv7.getLong2());
        assertEquals(Optional.of(3.17f), fv7.getFloat2());
        assertEquals(Optional.empty(), fv7.getBoolean2());
        assertEquals(Optional.empty(), fv7.getInteger2());
        assertEquals(Optional.empty(), fv7.getString2());
        assertEquals(Optional.empty(), fv7.getOffset());

        FooVariant11 fv8 = FooVariant11.ofBoolean2(true);
        assertEquals(Optional.empty(), fv8.getLong1());
        assertEquals(Optional.empty(), fv8.getFloat1());
        assertEquals(Optional.empty(), fv8.getBoolean1());
        assertEquals(Optional.empty(), fv8.getInteger1());
        assertEquals(Optional.empty(), fv8.getString1());
        assertEquals(Optional.empty(), fv8.getLong2());
        assertEquals(Optional.empty(), fv8.getFloat2());
        assertEquals(Optional.of(true), fv8.getBoolean2());
        assertEquals(Optional.empty(), fv8.getInteger2());
        assertEquals(Optional.empty(), fv8.getString2());
        assertEquals(Optional.empty(), fv8.getOffset());

        FooVariant11 fv9 = FooVariant11.ofInteger2(49);
        assertEquals(Optional.empty(), fv9.getLong1());
        assertEquals(Optional.empty(), fv9.getFloat1());
        assertEquals(Optional.empty(), fv9.getBoolean1());
        assertEquals(Optional.empty(), fv9.getInteger1());
        assertEquals(Optional.empty(), fv9.getString1());
        assertEquals(Optional.empty(), fv9.getLong2());
        assertEquals(Optional.empty(), fv9.getFloat2());
        assertEquals(Optional.empty(), fv9.getBoolean2());
        assertEquals(Optional.of(49), fv9.getInteger2());
        assertEquals(Optional.empty(), fv9.getString2());
        assertEquals(Optional.empty(), fv9.getOffset());

        FooVariant11 fv10 = FooVariant11.ofString2("jkl");
        assertEquals(Optional.empty(), fv10.getLong1());
        assertEquals(Optional.empty(), fv10.getFloat1());
        assertEquals(Optional.empty(), fv10.getBoolean1());
        assertEquals(Optional.empty(), fv10.getInteger1());
        assertEquals(Optional.empty(), fv10.getString1());
        assertEquals(Optional.empty(), fv10.getLong2());
        assertEquals(Optional.empty(), fv10.getFloat2());
        assertEquals(Optional.empty(), fv10.getBoolean2());
        assertEquals(Optional.empty(), fv10.getInteger2());
        assertEquals(Optional.of("jkl"), fv10.getString2());
        assertEquals(Optional.empty(), fv10.getOffset());

        FooVariant11 fv11 =
            FooVariant11.ofOffset(new Offset(new UUID(21, 22), 23));
        assertEquals(Optional.empty(), fv11.getLong1());
        assertEquals(Optional.empty(), fv11.getFloat1());
        assertEquals(Optional.empty(), fv11.getBoolean1());
        assertEquals(Optional.empty(), fv11.getInteger1());
        assertEquals(Optional.empty(), fv11.getString1());
        assertEquals(Optional.empty(), fv11.getLong2());
        assertEquals(Optional.empty(), fv11.getFloat2());
        assertEquals(Optional.empty(), fv11.getBoolean2());
        assertEquals(Optional.empty(), fv11.getInteger2());
        assertEquals(Optional.empty(), fv11.getString2());
        assertEquals(Optional.of(new Offset(new UUID(21, 22), 23)),
                     fv11.getOffset());
    }
}
