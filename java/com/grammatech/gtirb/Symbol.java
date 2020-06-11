/*
 *  Copyright (C) 2020 GrammaTech, Inc.
 *
 *  This code is licensed under the MIT license. See the LICENSE file in the
 *  project root for license terms.
 *
 *  This project is sponsored by the Office of Naval Research, One Liberty
 *  Center, 875 N. Randolph Street, Arlington, VA 22203 under contract #
 *  N68335-17-C-0700.  The content of the information does not necessarily
 *  reflect the position or policy of the Government and no official
 *  endorsement should be inferred.
 *
 */

package com.grammatech.gtirb;

import java.util.UUID;

import com.grammatech.gtirb.proto.ModuleOuterClass;
import com.grammatech.gtirb.proto.SymbolOuterClass;

public class Symbol extends Node {
    private String name;
    private UUID referentUuid;
    private long value;
    private long address;
    private com.grammatech.gtirb.proto.SymbolOuterClass.Symbol protoSymbol;
    private com.grammatech.gtirb.proto.SymbolOuterClass.Symbol
        .OptionalPayloadCase payload_type;

    public Symbol(
        com.grammatech.gtirb.proto.SymbolOuterClass.Symbol protoSymbol) {

        this.protoSymbol = protoSymbol;
        super.setUuid(Util.byteStringToUuid(protoSymbol.getUuid()));
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
            // payload is optional.
            this.referentUuid = Util.NIL_UUID;
            this.value = 0;
        }
    }

    public String getName() { return name; }

    public void setName(String name) { this.name = name; }

    public Node getReferentUuid() { return Node.getByUuid(this.referentUuid); }

    public UUID getReferentByUuid() { return this.referentUuid; }

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

    public long getAddress() { return address; }

    public void setAddress(long address) { this.address = address; }

    public SymbolOuterClass.Symbol getProtoSymbol() { return this.protoSymbol; }
}
