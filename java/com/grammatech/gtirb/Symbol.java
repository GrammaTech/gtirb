/*
 *  Copyright (C) 2020-2023 GrammaTech, Inc.
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
import java.io.IOException;
import java.util.Optional;
import java.util.OptionalLong;
import java.util.UUID;

/**
 * Maps a name to an object in the IR.
 * A Symbol may have a referent or a value, or neither. A referent is
 * a ByteBlock, somewhere in the IR. A value is an integer.
 * Use the hasValue() method to determine if the payload is a value,
 * hasReferent to determine if it has a referent.
 */
public class Symbol extends Node {

    /**
     * 	Symbol payload options.
     */
    public enum PayloadType { REFERENT, VALUE, NONE }

    private Optional<Module> module;
    private String name;
    private long value;
    private UUID referentUuid;
    private PayloadType payloadType;
    private boolean atEnd;

    /**
     * Class constructor for a Symbol from a protobuf symbol.
     * @param  protoSymbol   The Symbol as serialized into a protocol buffer.
     */
    private Symbol(SymbolOuterClass.Symbol protoSymbol) throws IOException {
        super(Util.byteStringToUuid(protoSymbol.getUuid()));
        this.module = Optional.empty();
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
    }

    /**
     * Class constructor for a Symbol with a referent payload.
     * @param  name          The section as serialized into a protocol buffer.
     * @param  referentUuid  The symbol referent as a UUID.
     */
    public Symbol(String name, UUID referentUuid) {
        super();
        this.module = Optional.empty();
        this.name = name;
        this.referentUuid = referentUuid;
        this.payloadType = PayloadType.REFERENT;
        this.value = 0;
        this.atEnd = false; // default to not being at end
    }

    /**
     * Class constructor for a Symbol with a value payload.
     * @param  name          The section as serialized into a protocol buffer.
     * @param  value         The symbol value.
     */
    public Symbol(String name, long value) {
        super();
        this.module = Optional.empty();
        this.name = name;
        this.value = value;
        this.payloadType = PayloadType.VALUE;
        this.referentUuid = Util.NIL_UUID;
        this.atEnd = false; // default to not being at end
    }

    /**
     * Class constructor for a minimal Symbol with no payload.
     * @param  name          The section as serialized into a protocol buffer.
     */
    public Symbol(String name) {
        super();
        this.module = Optional.empty();
        this.name = name;
        this.payloadType = PayloadType.NONE;
        this.value = 0;
        this.referentUuid = Util.NIL_UUID;
        this.atEnd = false; // default to not being at end
    }

    /**
     * Get the {@link Module} this Symbol belongs to.
     *
     * @return  An Optional that contains the Module this
     * symbol belongs to, or empty if it does not belong to a Module.
     */
    public Optional<Module> getModule() { return this.module; }

    /**
     * Set the Module this Symbol belongs to.
     *
     * @param  An Optional that contains the Module this
     * symbol belongs to, or empty if it does not belong to a Module.
     */
    void setModule(Optional<Module> module) { this.module = module; }

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
    public Node getReferent() {
        if (this.payloadType == PayloadType.REFERENT)
            return Node.getByUuid(this.referentUuid);
        return null;
    }

    /**
     * Get the UUID of this Symbol's referent.
     *
     * @return  An Optional that has the UUID of this symbol's referent, if it
     * has one.
     */
    public Optional<UUID> getReferentUuid() {
        if (this.payloadType == PayloadType.REFERENT)
            return Optional.of(this.referentUuid);
        return Optional.empty();
    }

    /**
     * Set the UUID of this Symbol's referent.
     *
     * Calling this method will set the payload type to REFERENT.
     *
     * @param uuid  The UUID of this symbol's referent..
     */
    public void setReferentUuid(UUID uuid) {
        this.referentUuid = uuid;
        this.payloadType = PayloadType.REFERENT;
    }

    /**
     * Get the value of this Symbol.
     *
     * @return  An OptionalLong that has the symbol value, if it has one.
     */
    public OptionalLong getValue() {
        if (this.payloadType == PayloadType.VALUE)
            return OptionalLong.of(this.value);
        return OptionalLong.empty();
    }

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

    /**
     * Get whether symbol is at end.
     *
     * @return true if symbol is at end, false otherwise.
     */
    public boolean isAtEnd() { return this.atEnd; }

    /**
     * Set whether symbol is at end.

     * @param atEnd The at end value to set
     */
    public void setAtEnd(boolean atEnd) { this.atEnd = atEnd; }

    /**
     * De-serialize a {@link Symbol} from a protobuf .
     *
     * @param protoSymbol The protobuf version of this symbol
     * @return An initialized symbol.
     */
    static Symbol fromProtobuf(SymbolOuterClass.Symbol protoSymbol)
        throws IOException {
        return new Symbol(protoSymbol);
    }

    /**
     * Serialize this Symbol into a protobuf.
     *
     * @return Symbol protocol buffer.
     */
    SymbolOuterClass.Symbol.Builder toProtobuf() {
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
