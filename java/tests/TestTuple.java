package tests;

import static org.junit.jupiter.api.Assertions.*;

import com.grammatech.gtirb.tuple.Tuple1;
import com.grammatech.gtirb.tuple.Tuple2;
import com.grammatech.gtirb.tuple.Tuple3;
import com.grammatech.gtirb.tuple.Tuple4;
import com.grammatech.gtirb.tuple.Tuple5;
import org.junit.jupiter.api.Test;

class BarSingle extends Tuple1<String> {
    public BarSingle(String s) { super(s); }
    public String getString() { return this.get0(); }
}

class BarPair extends Tuple2<String, Long> {
    public BarPair(String s, Long l) { super(s, l); }
    public String getString() { return this.get0(); }
    public Long getLong() { return this.get1(); }
}

class BarTriple extends Tuple3<String, Long, Float> {
    public BarTriple(String s, Long l, Float f) { super(s, l, f); }
    public String getString() { return this.get0(); }
    public Long getLong() { return this.get1(); }
    public Float getFloat() { return this.get2(); }
}

class BarQuadruple extends Tuple4<String, Long, Float, Boolean> {
    public BarQuadruple(String s, Long l, Float f, Boolean b) {
        super(s, l, f, b);
    }
    public String getString() { return this.get0(); }
    public Long getLong() { return this.get1(); }
    public Float getFloat() { return this.get2(); }
    public Boolean getBoolean() { return this.get3(); }
}

class BarQuintuple extends Tuple5<String, Long, Float, Boolean, Integer> {
    public BarQuintuple(String s, Long l, Float f, Boolean b, Integer i) {
        super(s, l, f, b, i);
    }
    public String getString() { return this.get0(); }
    public Long getLong() { return this.get1(); }
    public Float getFloat() { return this.get2(); }
    public Boolean getBoolean() { return this.get3(); }
    public Integer getInteger() { return this.get4(); }
}

public class TestTuple {
    @Test
    public void testTuple1() {
        BarSingle f1 = new BarSingle("abc");
        assertEquals("abc", f1.getString());
        BarSingle f2 = new BarSingle("abc");
        assertEquals(f1, f2);
        BarSingle f3 = new BarSingle("def");
        assertNotEquals(f1, f3);
    }

    @Test
    public void testTuple2() {
        BarPair f1 = new BarPair("abc", 42L);
        assertEquals("abc", f1.getString());
        assertEquals(42L, f1.getLong());
        BarPair f2 = new BarPair("abc", 42L);
        assertEquals(f1, f2);
        BarPair f3 = new BarPair("def", 43L);
        assertNotEquals(f1, f3);
        BarPair f4 = new BarPair("abc", 43L);
        assertNotEquals(f1, f4);
    }

    @Test
    public void testTuple3() {
        BarTriple f1 = new BarTriple("abc", 42L, 3.14f);
        assertEquals("abc", f1.getString());
        assertEquals(42L, f1.getLong());
        assertEquals(3.14f, f1.getFloat());
        BarTriple f2 = new BarTriple("abc", 42L, 3.14f);
        assertEquals(f1, f2);
        BarTriple f3 = new BarTriple("def", 42L, 3.14f);
        assertNotEquals(f1, f3);
        BarTriple f4 = new BarTriple("abc", 43L, 3.14f);
        assertNotEquals(f1, f4);
        BarTriple f5 = new BarTriple("abc", 42L, 3.15f);
        assertNotEquals(f1, f5);
    }

    @Test
    public void testTuple4() {
        BarQuadruple f1 = new BarQuadruple("abc", 42L, 3.14f, true);
        assertEquals("abc", f1.getString());
        assertEquals(42L, f1.getLong());
        assertEquals(3.14f, f1.getFloat());
        assertEquals(true, f1.getBoolean());
        BarQuadruple f2 = new BarQuadruple("abc", 42L, 3.14f, true);
        assertEquals(f1, f2);
        BarQuadruple f3 = new BarQuadruple("def", 42L, 3.14f, true);
        assertNotEquals(f1, f3);
        BarQuadruple f4 = new BarQuadruple("abc", 43L, 3.14f, true);
        assertNotEquals(f1, f4);
        BarQuadruple f5 = new BarQuadruple("abc", 42L, 3.15f, true);
        assertNotEquals(f1, f5);
        BarQuadruple f6 = new BarQuadruple("abc", 42L, 3.14f, false);
        assertNotEquals(f1, f6);
    }

    @Test
    public void testTuple5() {
        BarQuintuple f1 = new BarQuintuple("abc", 42L, 3.14f, true, 10);
        assertEquals("abc", f1.getString());
        assertEquals(42L, f1.getLong());
        assertEquals(3.14f, f1.getFloat());
        assertEquals(true, f1.getBoolean());
        assertEquals(10, f1.getInteger());
        BarQuintuple f2 = new BarQuintuple("abc", 42L, 3.14f, true, 10);
        assertEquals(f1, f2);
        BarQuintuple f3 = new BarQuintuple("def", 42L, 3.14f, true, 10);
        assertNotEquals(f1, f3);
        BarQuintuple f4 = new BarQuintuple("abc", 43L, 3.14f, true, 10);
        assertNotEquals(f1, f4);
        BarQuintuple f5 = new BarQuintuple("abc", 42L, 3.15f, true, 10);
        assertNotEquals(f1, f5);
        BarQuintuple f6 = new BarQuintuple("abc", 42L, 3.14f, false, 10);
        assertNotEquals(f1, f6);
        BarQuintuple f7 = new BarQuintuple("abc", 42L, 3.14f, true, 11);
        assertNotEquals(f1, f7);
    }
}
