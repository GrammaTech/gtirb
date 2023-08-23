package tests;

import static org.junit.jupiter.api.Assertions.*;

import com.grammatech.gtirb.tuple.Tuple2;
import com.grammatech.gtirb.tuple.Tuple3;
import com.grammatech.gtirb.tuple.Tuple4;
import com.grammatech.gtirb.tuple.Tuple5;
import org.junit.jupiter.api.Test;

class FooPair extends Tuple2<String, Long> {
    public FooPair(String s, Long l) { super(s, l); }
    public String getString() { return this.get0(); }
    public Long getLong() { return this.get1(); }
}

class FooTriple extends Tuple3<String, Long, Float> {
    public FooTriple(String s, Long l, Float f) { super(s, l, f); }
    public String getString() { return this.get0(); }
    public Long getLong() { return this.get1(); }
    public Float getFloat() { return this.get2(); }
}

class FooQuadruple extends Tuple4<String, Long, Float, Boolean> {
    public FooQuadruple(String s, Long l, Float f, Boolean b) {
        super(s, l, f, b);
    }
    public String getString() { return this.get0(); }
    public Long getLong() { return this.get1(); }
    public Float getFloat() { return this.get2(); }
    public Boolean getBoolean() { return this.get3(); }
}

class FooQuintuple extends Tuple5<String, Long, Float, Boolean, Integer> {
    public FooQuintuple(String s, Long l, Float f, Boolean b, Integer i) {
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
    public void testTuple2() {
        FooPair f1 = new FooPair("abc", 42L);
        assertEquals("abc", f1.getString());
        assertEquals(42L, f1.getLong());
        FooPair f2 = new FooPair("abc", 42L);
        assertEquals(f1, f2);
        FooPair f3 = new FooPair("def", 43L);
        assertNotEquals(f1, f3);
        FooPair f4 = new FooPair("abc", 43L);
        assertNotEquals(f1, f4);
    }

    @Test
    public void testTuple3() {
        FooTriple f1 = new FooTriple("abc", 42L, 3.14f);
        assertEquals("abc", f1.getString());
        assertEquals(42L, f1.getLong());
        assertEquals(3.14f, f1.getFloat());
        FooTriple f2 = new FooTriple("abc", 42L, 3.14f);
        assertEquals(f1, f2);
        FooTriple f3 = new FooTriple("def", 42L, 3.14f);
        assertNotEquals(f1, f3);
        FooTriple f4 = new FooTriple("abc", 43L, 3.14f);
        assertNotEquals(f1, f4);
        FooTriple f5 = new FooTriple("abc", 42L, 3.15f);
        assertNotEquals(f1, f5);
    }

    @Test
    public void testTuple4() {
        FooQuadruple f1 = new FooQuadruple("abc", 42L, 3.14f, true);
        assertEquals("abc", f1.getString());
        assertEquals(42L, f1.getLong());
        assertEquals(3.14f, f1.getFloat());
        assertEquals(true, f1.getBoolean());
        FooQuadruple f2 = new FooQuadruple("abc", 42L, 3.14f, true);
        assertEquals(f1, f2);
        FooQuadruple f3 = new FooQuadruple("def", 42L, 3.14f, true);
        assertNotEquals(f1, f3);
        FooQuadruple f4 = new FooQuadruple("abc", 43L, 3.14f, true);
        assertNotEquals(f1, f4);
        FooQuadruple f5 = new FooQuadruple("abc", 42L, 3.15f, true);
        assertNotEquals(f1, f5);
        FooQuadruple f6 = new FooQuadruple("abc", 42L, 3.14f, false);
        assertNotEquals(f1, f6);
    }

    @Test
    public void testTuple5() {
        FooQuintuple f1 = new FooQuintuple("abc", 42L, 3.14f, true, 10);
        assertEquals("abc", f1.getString());
        assertEquals(42L, f1.getLong());
        assertEquals(3.14f, f1.getFloat());
        assertEquals(true, f1.getBoolean());
        assertEquals(10, f1.getInteger());
        FooQuintuple f2 = new FooQuintuple("abc", 42L, 3.14f, true, 10);
        assertEquals(f1, f2);
        FooQuintuple f3 = new FooQuintuple("def", 42L, 3.14f, true, 10);
        assertNotEquals(f1, f3);
        FooQuintuple f4 = new FooQuintuple("abc", 43L, 3.14f, true, 10);
        assertNotEquals(f1, f4);
        FooQuintuple f5 = new FooQuintuple("abc", 42L, 3.15f, true, 10);
        assertNotEquals(f1, f5);
        FooQuintuple f6 = new FooQuintuple("abc", 42L, 3.14f, false, 10);
        assertNotEquals(f1, f6);
        FooQuintuple f7 = new FooQuintuple("abc", 42L, 3.14f, true, 11);
        assertNotEquals(f1, f7);
    }
}
