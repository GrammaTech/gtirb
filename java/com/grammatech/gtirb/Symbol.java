/*
 *  Copyright (C) 2020-2021 GrammaTech, Inc.
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

import com.grammatech.gtirb.proto.SymbolOuterClass;
import java.util.UUID;

/**
 * The Symbol class maps a name to an object in the IR.
 * A Symbol may have a referent or a value, or neither. A referent is
 * a ByteBlock, somewhere in the IR. A value is an integer.
 * Use the hasValue() method to determine if the payload is a value,
 * hasReferent to determine if it has a referent.
 */
public class Symbol extends Node {

    public enum PayloadType { REFERENT, VALUE, NONE }

    private String name;
    private long value;
    private UUID referentUuid;
    private PayloadType payloadType;
    private Module module;
    private boolean atEnd;
    private SymbolOuterClass.Symbol protoSymbol;

    /**
     * Class constructor for a Symbol from a protobuf symbol.
     * @param  protoSymbol   The Symbol as serialized into a protocol buffer.
     * @param  module        The Module that owns this Section.
     */
    public Symbol(SymbolOuterClass.Symbol protoSymbol, Module module) {
        this.protoSymbol = protoSymbol;
        this.uuid = Util.byteStringToUuid(protoSymbol.getUuid());
        this.name = protoSymbol.getName();
        this.atEnd = protoSymbol.getAtEnd();
        // Set default values:
        this.value = 0;
        this.referentUuid = Util.NIL_UUID; // init to null as fallback
        // Determine payload for this symbol, if there is one:
        if (protoSymbol.getOptionalPayloadCase() ==
            SymbolOuterClass.Symbol.OptionalPayloadCase.VALUE) {
            this.value = protoSymbol.getValue();
            this.payloadType = PayloadType.VALUE;
        } else if (protoSymbol.getOptionalPayloadCase() ==
                   SymbolOuterClass.Symbol.OptionalPayloadCase.REFERENT_UUID) {
            this.referentUuid =
                Util.byteStringToUuid(protoSymbol.getReferentUuid());
            this.payloadType = PayloadType.REFERENT;
        } else {
            this.payloadType = PayloadType.NONE;
        }
        this.module = module;
    }

    /**
     * Class constructor for a Symbol with a referent payload.
     * @param  name          The section as serialized into a protocol buffer.
     * @param  referentUuid  The symbol referent as a UUID.
     * @param  module        The Module that owns this Section.
     */
    public Symbol(String name, UUID referentUuid, Module module) {
        this.name = name;
        this.referentUuid = referentUuid;
        this.payloadType = PayloadType.REFERENT;
        this.value = 0;
        this.module = module;
        this.atEnd = false; // default to not being at end
    }

    /**
     * Class constructor for a Symbol with a value payload.
     * @param  name          The section as serialized into a protocol buffer.
     * @param  value         The symbol value.
     * @param  module        The Module that owns this Section.
     */
    public Symbol(String name, long value, Module module) {
        this.name = name;
        this.value = value;
        this.payloadType = PayloadType.VALUE;
        this.referentUuid = Util.NIL_UUID;
        this.module = module;
        this.atEnd = false; // default to not being at end
    }

    /**
     * Class constructor for a Symbol with no payload.
     * @param  name          The section as serialized into a protocol buffer.
     * @param  module        The Module that owns this Section.
     */
    public Symbol(String name, Module module) {
        this.name = name;
        this.payloadType = PayloadType.NONE;
        this.value = 0;
        this.referentUuid = Util.NIL_UUID;
        this.module = module;
        this.atEnd = false; // default to not being at end
    }

    /**
     * Get the name of this Symbol.
     *
     * @return  The symbol name.
     */
    public String getName() { return name; }

    /**
     * Set the name of this Symbol.
     *
     * @param name    The symbol name.
     */
    public void setName(String name) { this.name = name; }

    /**
     * Get the referent of this Symbol.
     *
     * @return  The symbol's referent, or null if there is no referent.
     */
    public Node getReferentUuid() {
        if (this.payloadType == PayloadType.REFERENT)
            return Node.getByUuid(this.referentUuid);
        return null;
    }

    /**
     * Get the UUID of this Symbol's referent.
     *
     * @return  The UUID of this symbol's referent, or NIL UUID if there is no
     * referent.
     */
    public UUID getReferentByUuid() { return this.referentUuid; }

    /**
     * Set the UUID of this Symbol's referent.
     *
     * Calling this method will set the payload type to REFERENT.
     *
     * @param uuid  The UUID of this symbol's referent..
     */
    public void setReferentByUuid(UUID uuid) {
        this.referentUuid = uuid;
        this.payloadType = PayloadType.REFERENT;
    }

    /**
     * Get the value of this Symbol.
     *
     * @return  The symbol name.
     */
    public long getValue() { return this.value; }

    /**
     * Set the value of this Symbol.
     *
     * @param value   The symbol value.
     */
    public void setValue(long value) {
        this.value = value;
        this.payloadType = PayloadType.VALUE;
    }

    /**
     * Get the payload type of this Symbol.
     *
     * @return  The symbol name.
     */
    public PayloadType getPayloadType() { return this.payloadType; }

    // Deprecating this, replaced with above methods
    //    public boolean hasValue() {
    //        return (this.payload_type ==
    //        SymbolOuterClass
    //                                         .Symbol.OptionalPayloadCase.VALUE);
    //    }
    //
    //    /**
    //     * Get the name of this Symbol.
    //     *
    //     * @return  The symbol name.
    //     */
    //    public boolean hasReferent() {
    //        return (this.payload_type ==
    //                SymbolOuterClass.Symbol
    //                    .OptionalPayloadCase.REFERENT_UUID);
    //    }

    /**
     * Get the original protobuf of this {@link Symbol}.
     *
     * @return The protobuf the symbol was imported from, or
     * null if it was not imported from a protobuf.
     */
    public SymbolOuterClass.Symbol getProtoSymbol() { return this.protoSymbol; }

    /**
     * Get the Module that owns this Symbol.
     *
     * @return the module
     */
    public Module getModule() { return module; }

    /**
     * Get whether symbol is at end.
     *
     * @return true if symbol is at end, false otherwise.
     */
    public boolean isAtEnd() { return atEnd; }

    /**
     * Set whether symbol is at end.

     * @param atEnd The at end value to set
     */
    public void setAtEnd(boolean atEnd) { this.atEnd = atEnd; }

    /**
     * De-serialize a {@link Symbol} from a protobuf .
     *
     * @param protoSymbol The protobuf version of this symbol
     * @param module The module this symbols to, or null if none.
     * @return An initialized symbol.
     */
    public static Symbol fromProtobuf(SymbolOuterClass.Symbol protoSymbol,
                                      Module module) {
        return new Symbol(protoSymbol, module);
    }

    /**
     * Serialize this Symbol into a protobuf.
     *
     * @return Symbol protocol buffer.
     */
    public SymbolOuterClass.Symbol.Builder toProtobuf() {
        SymbolOuterClass.Symbol.Builder protoSymbol =
            SymbolOuterClass.Symbol.newBuilder();
        protoSymbol.setUuid(Util.uuidToByteString(this.getUuid()));
        protoSymbol.setName(this.getName());
        protoSymbol.clearOptionalPayload();
        if (this.payloadType == PayloadType.VALUE)
            protoSymbol.setValue(this.value);
        else if (this.payloadType == PayloadType.REFERENT)
            protoSymbol.setReferentUuid(
                Util.uuidToByteString(this.referentUuid));
        return protoSymbol;
    }
}
