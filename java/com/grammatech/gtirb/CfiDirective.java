package com.grammatech.gtirb;

import com.grammatech.gtirb.tuple.Tuple3;
import java.util.List;
import java.util.UUID;

/**
 * A tuple representing an individual CFI directive.
 */
public class CfiDirective extends Tuple3<String, List<Long>, UUID> {
    /**
     * Constructor for a CFI directive.
     *
     * @param name The name of the directive (mnemonic.)
     * @param args Numeric arguments for the directive.
     * @param symbolUuid UUID of the {@link Symbol} indicating where the
     *     directive applies.
     */
    public CfiDirective(String name, List<Long> args, UUID symbolUuid) {
        super(name, args, symbolUuid);
    }

    /**
     * Get the name of the directive (mnemonic.)
     */
    public String getName() { return this.get0(); }

    /**
     * Get the numeric arguments to the directive.
     */
    public List<Long> getArgs() { return this.get1(); }

    /**
     * Get the UUID of the {@link Symbol} indicating where the directive
     * applies.
     */
    public UUID getSymbolUuid() { return this.get2(); }
}
