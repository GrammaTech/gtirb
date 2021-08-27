package com.grammatech.gtirb;

import java.util.Map;
import java.util.UUID;

/**
 * Schema class for symbolForwarding auxiliary data.
 * Key: The UUID of the "from" {@link Symbol}.
 * Value: The UUID of the "to" {@link Symbol}.
 * Attached To: Module
 */
public class SymbolForwarding {
    private Map<UUID, UUID> map;

    /**
     * Class Constructor.
     * @param  map  The map of symbol UUID to symbol UUID.
     */
    public SymbolForwarding(Map<UUID, UUID> map) { this.map = map; }

    /**
     * Get a symbol forwarding
     *
     * @param fromUuid The UUID of the "from" symbol.
     * @return The UUID of the "to" symbol.
     */
    public UUID getSymbolForwarding(UUID fromUuid) {
        return this.map.get(fromUuid);
    }

    /**
     * Set a symbol forwarding.
     *
     * @param fromUuid The UUID of the "from" symbol.
     * @param toUuid The UUID of the "from" symbol.
     */
    public void setSymbolForwarding(UUID fromUuid, UUID toUuid) {
        this.map.put(fromUuid, toUuid);
    }

    /**
     * Get the symbol forwarding map.
     *
     * @return  The map of symbol UUID to symbol UUID.
     */
    public Map<UUID, UUID> getMap() { return this.map; }

    /**
     * Set the symbol forwarding map.
     *
     * @param  map  The map of symbol UUID to symbol UUID.
     */
    public void setMap(Map<UUID, UUID> map) { this.map = map; }
}
