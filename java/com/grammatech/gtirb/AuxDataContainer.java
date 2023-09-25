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

import com.google.protobuf.ByteString;
import com.grammatech.gtirb.proto.AuxDataOuterClass;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.IllegalArgumentException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * Provides functionality for associating auxiliary
 * data with elements of the intermediate representation.
 */
public abstract class AuxDataContainer extends Node {

    /**
     * Inner class for managing AuxData instances
     */
    public static class AuxData {

        // Always populated.
        private String name;
        private String typeName;

        // Only populated during serialization events.
        // This is considered stale if the schema/decoded members are non-empty.
        private Optional<byte[]> encoded;

        // Only populated if the client adds/gets the AuxData.
        private Optional<AuxDataSchema> schema;
        private Optional<Object> decoded;

        /**
         * Class constructor for AuxData from protobuf {@link AuxData}.
         * @param  protoAuxData   The {@link AuxData} as serialized into a
         *     protocol buffer.
         * @param  name           The name of this {@link AuxData}.
         */
        AuxData(String name, AuxDataOuterClass.AuxData protoAuxData) {
            this.name = name;
            this.typeName = protoAuxData.getTypeName();
            this.encoded = Optional.of(protoAuxData.getData().toByteArray());
            this.schema = Optional.empty();
            this.decoded = Optional.empty();
        }

        /**
         * Class constructor for {@link AuxData} from an in-memory object.
         * @param schema The {@link AuxDataSchema} for the AuxData entry.
         * @param value The value to associate with thie AuxData entry.
         */
        <T> AuxData(AuxDataSchema<T> schema, T value) {
            this.name = schema.getName();
            this.typeName = schema.getCodec().getTypeName();
            this.encoded = Optional.empty();
            this.schema = Optional.of(schema);
            this.decoded = Optional.of(value);
        }

        /**
         * Get the {@link AuxData} name.
         *
         * @return the name.
         */
        public String getName() { return this.name; }

        /**
         * Get the Type String (schemata).
         *
         * @return the type string.
         */
        public String getTypeName() { return this.typeName; }

        /**
         * Get the decoded form of the {@link AuxData}.
         *
         * @param sch The schema used for decoding this {@link AuxData}.
         * @return The decoded data object for this {@link AuxData}.
         */
        public <T> T getDecodedData(AuxDataSchema<T> sch) throws IOException {
            // TODO: Some better way to confirm schema equivalence here.
            // In particular, one could have the correct name and type
            // name but still have an inconsistent type for T. Specifically,
            // we want to test if sch is equivalent to this.schema.get().

            // If this is not true, there's something seriously wrong with
            // the AuxDataContainer code.
            assert this.name.equals(sch.getName());

            // This could be incorrect if the client is using inconsistent
            // schemas with the same schema name.
            if (!this.typeName.equals(sch.getCodec().getTypeName())) {
                throw new IllegalArgumentException(
                    "Schema type names do not match! " + this.typeName +
                    " vs. " + sch.getCodec().getTypeName());
            }

            if (!this.schema.isPresent()) {
                // If we're here because this is the initial get, and the
                // AuxData has not been unserialized yet. Do the decoding now.
                assert this.encoded.isPresent();
                this.schema = Optional.of(sch);
                this.decoded = Optional.of(sch.getCodec().decode(
                    new ByteArrayInputStream(this.encoded.get())));
            }

            return (T)this.decoded.get();
        }

        /**
         * Serialize this AuxData into a protobuf .
         *
         * @return AuxData protocol buffer.
         */
        AuxDataOuterClass.AuxData.Builder toProtobuf() {
            // If we have a schema and decoded object, encode first.
            if (this.schema.isPresent()) {
                assert this.decoded.isPresent();
                ByteArrayOutputStream os = new ByteArrayOutputStream();

                // ByteArrayOutputStream shouldn't ever throw, but
                // because we're passing it through the OutputStream,
                // we have a syntactic obligation to check for throws.
                try {
                    this.schema.get().getCodec().encode(os, this.decoded.get());
                } catch (Exception e) {
                    assert false;
                }

                this.encoded = Optional.of(os.toByteArray());
            } else {
                assert this.encoded.isPresent();
            }
            AuxDataOuterClass.AuxData.Builder protoAuxData =
                AuxDataOuterClass.AuxData.newBuilder();
            protoAuxData.setData(ByteString.copyFrom(this.encoded.get()));
            protoAuxData.setTypeName(this.typeName);
            return protoAuxData;
        }
    }

    protected HashMap<String, AuxData> auxDataMap;

    /**
     * Class constructor for an AuxDataContainer from a protobuf AuxData Map.
     * @param  protoUuid        The UUID of this container.
     * @param  protoAuxDataMap  A Map of AuxData names to protobuf AuxData
     * objects.
     */
    AuxDataContainer(ByteString protoUuid,
                     Map<String, AuxDataOuterClass.AuxData> protoAuxDataMap)
        throws IOException {
        super(Util.byteStringToUuid(protoUuid));
        this.auxDataMap = new HashMap<String, AuxData>();
        if (protoAuxDataMap != null) {
            for (Map.Entry<String, AuxDataOuterClass.AuxData> entry :
                 protoAuxDataMap.entrySet()) {
                AuxData ad = new AuxData(entry.getKey(), entry.getValue());
                auxDataMap.put(ad.getName(), ad);
            }
        }
    }

    AuxDataContainer() {
        super();
        this.auxDataMap = new HashMap<String, AuxData>();
    }

    /**
     * Retrieve an arbitrary {@link AuxData} item from this container if it
     * exists.
     *
     * @param schema The schema for the AuxData
     * @return An {@link AuxData} object, or empty() if not present.
     */
    public <T> Optional<T> getAuxData(AuxDataSchema<T> schema) {
        try {
            AuxData ad = this.auxDataMap.get(schema.getName());

            if (ad == null) {
                return Optional.empty();
            } else {
                return Optional.of(ad.getDecodedData(schema));
            }
        } catch (IOException e) {
            // This can occur when either the serialized content of the AuxData
            // was corrupt or if the schema we're using is incompatible with it.
            // In both cases, treat the the AuxData as not available.
            return Optional.empty();
        }
    }

    /**
     * Adds an arbitrary {@link AuxData} item to this container. If an AuxData
     * already exists with the given schema, then it is overwritten.
     *
     * @param schema The schema to use for the data
     * @param data The data to add
     */
    public <T> void putAuxData(AuxDataSchema<T> schema, T data) {
        AuxData ad = new AuxData(schema, data);
        this.auxDataMap.put(schema.getName(), ad);
    }

    /**
     * Remove an {@link AuxData} from this container.
     *
     * @param schema The schema of the {@link AuxData} to remove.
     * @return False if the {@link AuxData} was not present in the container.
     *     True otherwise.
     */
    public boolean removeAuxData(AuxDataSchema<?> schema) {
        return this.removeAuxData(schema.getName());
    }

    /**
     * Remove an {@link AuxData} from this container.
     *
     * This version of the function can be used for AuxData for which
     * the schema is not known locally.
     *
     * @param name The name of the {@link AuxData} to remove.
     * @return False if the {@link AuxData} was not present in the container.
     *     True otherwise.
     */
    public boolean removeAuxData(String name) {
        AuxData ad = this.auxDataMap.remove(name);
        return ad != null;
    }

    /**
     * Remove all {@link AuxData} from this container.
     */
    public void clearAuxData() { this.auxDataMap.clear(); }

    /**
     * Get a view of the {@link AuxData} entries present in this container.
     *
     * @return An unmodifiable view of the map of {@link AuxData} entries in
     *     this container indexed by name.
     */
    public Map<String, AuxData> getAuxDataMap() {
        return Collections.unmodifiableMap(this.auxDataMap);
    }
}
