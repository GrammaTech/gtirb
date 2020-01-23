/** */
package com.grammatech.gtirb;

import java.util.UUID;

public class Symbol extends Node {
    private String name;
    private UUID referentUuid;
    private long value;
    private com.grammatech.gtirb.proto.SymbolOuterClass.Symbol
        .OptionalPayloadCase payload_type;

    public Symbol(
        com.grammatech.gtirb.proto.SymbolOuterClass.Symbol protoSymbol) {

        UUID uuid = Util.byteStringToUuid(protoSymbol.getUuid());
        super.setUuid(uuid);
        this.name = protoSymbol.getName();
        this.payload_type = protoSymbol.getOptionalPayloadCase();
        if (this.payload_type == com.grammatech.gtirb.proto.SymbolOuterClass
                                     .Symbol.OptionalPayloadCase.VALUE) {
            this.value = protoSymbol.getValue();
            this.referentUuid = Util.NIL_UUID;
        } else if (this.payload_type ==
                   com.grammatech.gtirb.proto.SymbolOuterClass.Symbol
                       .OptionalPayloadCase.REFERENT_UUID) {
            this.referentUuid =
                Util.byteStringToUuid(protoSymbol.getReferentUuid());
            this.value = 0;
        } else {
            this.referentUuid = Util.NIL_UUID;
            this.value = 0;
        }
    }

    public String getName() { return name; }

    public void setName(String name) { this.name = name; }

    public Node getReferentUuid() { return Node.getByUuid(this.referentUuid); }

    public long getValue() { return this.value; }

    public boolean hasValue() {
        return (this.payload_type == com.grammatech.gtirb.proto.SymbolOuterClass
                                         .Symbol.OptionalPayloadCase.VALUE);
    }

    public boolean hasReferent() {
        return (this.payload_type ==
                com.grammatech.gtirb.proto.SymbolOuterClass.Symbol
                    .OptionalPayloadCase.REFERENT_UUID);
    }
}
