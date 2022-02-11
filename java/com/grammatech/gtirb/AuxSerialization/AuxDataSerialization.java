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

package com.grammatech.gtirb.AuxSerialization;

import com.grammatech.gtirb.*;

import java.io.ByteArrayOutputStream;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

/**
 * Serialize and De-serialize AuxData.
 *
 * The objects returned by {@link #decode(byte[], String)} and accepted by
 * {@link #encode(Object, String)} always have a type that matches the type
 * string provided to those methods. The type string is language-independent
 * and shared by all implementations of GTIRB.
 *
 * Translation from GTIRB types to Java types:
 * <p><table>
 *     <thead>
 *         <tr><th>GTIRB type</th><th>Java type</th><th>Notes</th></tr>
 *     </thead>
 *     <tr>
 *         <td>sequence&lt;E&gt;</td>
 *         <td>{@link java.util.List}&lt;E&gt;</td>
 *     </tr>
 *     <tr>
 *         <td>set&lt;E&gt;</td>
 *         <td>{@link java.util.Set}&lt;E&gt;</td>
 *     </tr>
 *     <tr>
 *         <td>mapping&lt;K,V&gt;</td>
 *         <td>{@link java.util.Map}&lt;K,V&gt;</td>
 *     </tr>
 *     <tr>
 *         <td>tuple&lt;A,B&gt;</td>
 *         <td>{@link TwoTuple}&lt;A,B&gt;</td>
 *     </tr>
 *     <tr>
 *         <td>tuple&lt;A,B,C&gt;</td>
 *         <td>{@link ThreeTuple}&lt;A,B,C&gt;</td>
 *     </tr>
 *     <tr>
 *         <td>tuple&lt;A,B,C,D,E&gt;</td>
 *         <td>{@link FiveTuple}&lt;A,B,C,D,E&gt;</td>
 *     </tr>
 *     <tr>
 *         <td>tuple&lt;A,B,...&gt;</td>
 *         <td>{@link java.util.List}&lt;{@link Object}&gt;</td>
 *         <td>Any tuple size not listed above uses a generic List.</td>
 *     </tr>
 *     <tr><td>Offset</td><td>{@link Offset}</td><td></td></tr>
 *     <tr><td>UUID</td><td>{@link java.util.UUID}</td><td></td></tr>
 *     <tr><td>string</td><td>{@link String}</td><td></td></tr>
 *     <tr><td>uint64_t</td><td>{@link Long}</td><td></td></tr>
 *     <tr><td>int64_t</td><td>{@link Long}</td><td></td></tr>
 *     <tr><td>uint32_t</td><td>{@link Integer}</td><td></td></tr>
 *     <tr><td>int32_t</td><td>{@link Integer}</td><td></td></tr>
 *     <tr><td>uint8_t</td><td>{@link Byte}</td><td></td></tr>
 *     <tr><td>int8_t</td><td>{@link Byte}</td><td></td></tr>
 *     <tr><td>double</td><td>{@link Double}</td><td></td></tr>
 *     <tr><td>float</td><td>{@link Float}</td><td></td></tr>
 * </table></p>
 *
 * GTIRB types can be nested with a similar format to the corresponding
 * Java Generics. However, GTIRB AuxData type strings MUST NOT contain any
 * whitespace. A few examples:
 * <p><table>
 *     <thead>
 *         <tr><th>GTIRB type</th><th>Java type</th></tr>
 *     </thead>
 *     <tr>
 *         <td>{@code mapping<UUID,string>}</td>
 *         <td>{@code Map<UUID,String>}</td>
 *     </tr>
 *     <tr>
 *         <td>{@code set<tuple<string,uint64_t>>}</td>
 *         <td>{@code Set<TwoTuple<String,Long>>}</td>
 *     </tr>
 * </table></p>
 */
public class AuxDataSerialization {

    /**
     * Class Constructor
     */
    private AuxDataSerialization() {}

    public static Codec getCodec(String typeName) {
        switch (typeName) {
        case "sequence":
            return new SequenceCodec();
        case "mapping":
            return new MappingCodec();
        case "tuple":
            return new TupleCodec();
        case "set":
            return new SetCodec();
        case "Offset":
            return new OffsetCodec();
        case "int64_t":
        case "uint64_t":
            return new IntegerCodec(8);
        case "int32_t":
        case "uint32_t":
            return new IntegerCodec(4);
        case "int16_t":
        case "uint16_t":
            return new IntegerCodec(2);
        case "int8_t":
        case "uint8_t":
            return new IntegerCodec(1);
        case "double":
            return new FloatCodec(8);
        case "float":
            return new FloatCodec(4);
        case "string":
            return new StringCodec();
        case "UUID":
            return new UuidCodec();
        }
        throw new UnknownCodecException("Unknown type: " + typeName);
    }

    /**
     * Decode the data in 'byteBuffer' given a parsed type tree.
     *
     * @param byteBuffer   The binary stream to read bytes from.
     * @param parseTree    The parsed type of the object encoded by
     * 'byteBuffer'.
     * @return             Decoded AuxData according to parsed type tree.
     */
    static Object decodeTree(Serialization byteBuffer, AuxTypeTree parseTree) {
        String typeName = parseTree.getName();
        Codec codec = getCodec(typeName);
        return codec.decode(byteBuffer, parseTree.getChildren());
    }

    /**
     * Encode the {@link AuxData} given a parsed type tree.
     * @param outstream  A binary stream to write bytes to.
     * @param val        The {@link AuxData} to encode.
     * @param parseTree  The parsed type to encode with.
     */
    static void encodeTree(StreamSerialization outstream, Object val,
                           AuxTypeTree parseTree) {
        String typeName = parseTree.getName();
        Codec codec = getCodec(typeName);
        codec.encode(outstream, val, parseTree.getChildren());
    }

    /**
     * Decode auxdata from a byte array using a typeName
     *
     * @param rawBytes  The bytes from which to read the encoded value.
     * @param typeName  The type name of the object encoded by 'rawNytes'.
     * @return          The object encoded by 'rawBytes'.
     */
    public static Object decode(byte[] rawBytes, String typeName) {
        Object result;
        try {
            AuxTypeTree parseTree = AuxTypeTree.parseTypeString(typeName);
            Serialization byteBuffer = new Serialization(rawBytes);
            result = decodeTree(byteBuffer, parseTree);
        } catch (UnknownCodecException ex) {
            return new UnknownData(rawBytes);
        } catch (DecodeException ex) {
            throw new RuntimeException(ex.getMessage());
        }
        return result;
    }

    /**
     * Encode the value of an AuxData value to bytes.
     *
     * @param val       The {@link AuxData} to encode.
     * @param typeName  The type name of the value encapsulated by the {@link
     * AuxData}
     * @return          An encoded byte array.
     */
    public static byte[] encode(Object val, String typeName) {
        if (val instanceof UnknownData)
            return ((UnknownData)val).bytes;
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        StreamSerialization outstream = new StreamSerialization(baos);
        AuxTypeTree parseTree = AuxTypeTree.parseTypeString(typeName);
        if (parseTree == null) {
            throw new EncodeException("Error parsing AuxData type string: " +
                                      typeName);
        }
        try {
            encodeTree(outstream, val, parseTree);
        } catch (UnknownCodecException ex) {
            throw new EncodeException("unknown codec: " + parseTree.getName());
        }
        return baos.toByteArray();
    }
}
