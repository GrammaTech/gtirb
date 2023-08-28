package com.grammatech.gtirb;

import com.grammatech.gtirb.tuple.Tuple3;

/**
 * A tuple representing a probability fact associated with a function's name.
 */
public class ProbFuncName extends Tuple3<String, String, Float> {

    /**
     * Constructor.
     *
     * @param name Source-level function name.
     * @param binaryName Binary-level function name.
     * @param prob The probability associated with the names.
     */
    public ProbFuncName(String name, String binaryName, Float prob) {
        super(name, binaryName, prob);
    }

    /**
     * Get the source-level name.
     */
    public String getName() { return this.get0(); }

    /**
     * Get the binary-level name.
     */
    public String getBinaryName() { return this.get1(); }

    /**
     * Get the probability.
     */
    public Float getProbability() { return this.get2(); }
}
