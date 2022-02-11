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
import com.grammatech.gtirb.AuxSerialization.AuxDataSerialization;
import com.grammatech.gtirb.proto.AuxDataOuterClass;

/**
 * The AuxData class provides generic storage for application-specific data.
 * We specify a small number of standard schemata to support interoperability.
 */
public class AuxData {

    private String name;
    private String type;
    private byte[] data;

    /**
     * Class constructor for AuxData from protobuf AuxData.
     * @param  protoAuxData   The AuxData as serialized into a protocol buffer.
     * @param  name           The name of this AuxData.
     */
    public AuxData(AuxDataOuterClass.AuxData protoAuxData, String name) {
        this.data = protoAuxData.getData().toByteArray();
        this.name = name;
        this.type = protoAuxData.getTypeName();
    }

    /**
     * Class constructor for AuxData from data.
     * @param  data   The AuxData data as an array of bytes.
     * @param  name   The name of this AuxData.
     * @param  type   The type string (schemata) of this AuxData.
     */
    public AuxData(byte[] data, String name, String type) {
        this.data = data;
        this.name = name;
        this.type = type;
    }

    /**
     * Get the AuxData name.
     *
     * @return the name.
     */
    public String getName() { return name; }

    /**
     * Set the AuxData name.
     *
     * @param name the name to set.
     */
    public void setName(String name) { this.name = name; }

    /**
     * Get the Type String (schemata).
     *
     * @return the type string.
     */
    public String getType() { return type; }

    /**
     * Get the AuxData bytes.
     *
     * @return This AuxData as a byte array.
     */
    public byte[] getData() { return data; }

    /**
     * Set the AuxData bytes.
     *
     * @param data AuxData as a byte array.
     */
    public void setData(byte[] data) { this.data = data; }

    /**
     * Creates an AuxData object by serializing arbitrary data.
     * See {@link AuxDataSerialization} for more details about this
     * serialization.
     *
     * @param name The name of this AuxData.
     * @param type The type string (schemata) of this AuxData.
     * @param val  The object to be serialized
     * @return A new AuxData object
     */
    public static AuxData encode(String name, String type, Object val) {
        byte[] data = AuxDataSerialization.encode(val, type);
        return new AuxData(data, name, type);
    }

    /**
     * De-Serialize this AuxData from a protobuf .
     *
     * @param protoAuxData AuxData protocol buffer.
     * @param name The name of this AuxData.
     */
    public static AuxData fromProtobuf(AuxDataOuterClass.AuxData protoAuxData,
                                       String name) {
        return new AuxData(protoAuxData, name);
    }

    /**
     * Serialize this AuxData into a protobuf .
     *
     * @return AuxData protocol buffer.
     */
    public AuxDataOuterClass.AuxData.Builder toProtobuf() {
        AuxDataOuterClass.AuxData.Builder protoAuxData =
            AuxDataOuterClass.AuxData.newBuilder();
        protoAuxData.setData(ByteString.copyFrom(data));
        protoAuxData.setTypeName(this.type);
        return protoAuxData;
    }
}
