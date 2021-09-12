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

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.grammatech.gtirb.Serialization;
import com.grammatech.gtirb.TwoTuple;

/**
 *
 * Serialize and De-serialize AuxData.
 *
 */
public class AuxDataSerialization {

    /**
     * Class Constructor
     * @param raw_data
     */
    public AuxDataSerialization() {}

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
            return new IntegerCodec(8);
        case "uint64_t":
            return new IntegerCodec(8);
        case "int32_t":
            return new IntegerCodec(4);
        case "uint32_t":
            return new IntegerCodec(4);
        case "int16_t":
            return new IntegerCodec(2);
        case "uint16_t":
            return new IntegerCodec(2);
        case "int8_t":
            return new IntegerCodec(1);
        case "uint8_t":
            return new IntegerCodec(1);
        case "string":
            return new StringCodec();
        case "UUID":
            return new UuidCodec();
        }
        return null;
    }

    public ParsedProduct parse(List<String> tokens,
                               List<TwoTuple<String, Object>> tree) {
        // Is the list of tokens empty?
        // It is an error to parse nothing
        if (tokens.size() == 0) {
            System.err.println("parse error: no tokens! 1");
            return null;
        }

        // Create a list if tree is empty
        if (tree == null)
            tree = new ArrayList<TwoTuple<String, Object>>();

        LinkedList<String> tail = new LinkedList<String>(tokens);
        String firstToken = tail.pop();
        if (firstToken.equals("<") || firstToken.equals(">") ||
            firstToken.equals(",")) {
            System.err.println("parse error: type name error! 2");
            return null;
        }

        // # Base case
        // Tail empty means the token list had only one element
        // If so construct return value and return
        if (tail.size() == 0) {
            // Create a tuple having just this token and an empty subtree
            TwoTuple<String, Object> element = new TwoTuple<String, Object>(
                firstToken, Collections.emptyList());
            // Create a return value having this element as the subtree
            // and an empty list of strings as the remaining
            tree.add(element);
            // return new ParsedProduct(element, Collections.emptyList());
            return new ParsedProduct(tree, Collections.emptyList());
        }

        // Get next and subtract from the rest
        String nextToken = tail.pop();

        // No subtypes
        if (nextToken.equals(",")) {
            TwoTuple<String, Object> element = new TwoTuple<String, Object>(
                firstToken, Collections.emptyList());
            tree.add(element);
        }

        // Parse subtypes
        if (nextToken.equals("<")) {

            // Extract just the subtype tokens and parse them
            LinkedList<String> stack = new LinkedList<String>();
            LinkedList<String> subtypeTokens = new LinkedList<String>();
            LinkedList<String> remainingTokens = new LinkedList<String>();

            stack.add("<");

            for (String t : tail) {
                if (stack.size() == 0) {
                    remainingTokens.add(t);
                    continue;
                }
                if (t.equals("<")) {
                    stack.add(t);
                } else if (t.equals(">")) {
                    stack.pollLast();
                }
                subtypeTokens.add(t);
            }

            if (stack.size() > 0 || !subtypeTokens.getLast().equals(">")) {
                System.err.println("parse error: type name error! 3");
                return null;
            }

            List<TwoTuple<String, Object>> tupleList =
                new ArrayList<TwoTuple<String, Object>>();
            ParsedProduct soFar = parse(
                subtypeTokens.subList(0, subtypeTokens.size() - 1), tupleList);
            List<TwoTuple<String, Object>> subtree = soFar.getSubtree();
            List<String> remaining = soFar.getRemaining();

            // Parsing should consume all subtype tokens
            if (remaining.size() > 0) {
                System.err.println("parse error: type name error! 4");
                return null;
            }

            // Append this element and its subtree to the tree
            TwoTuple<String, Object> element =
                new TwoTuple<String, Object>(firstToken, subtree);
            tree.add(element);

            // Finish of all tokens are consumed
            if (remainingTokens.size() == 0) {
                return new ParsedProduct(tree, Collections.emptyList());
            }
            tail.clear();
            tail.addAll(remainingTokens);
            nextToken = tail.pop();
        }
        if (nextToken.equals(","))
            return parse(tail, tree);

        // To get here, None of the rules match, error
        System.err.println("parse error: type name error! 5");
        return null;
    }

    /**
     * parseType
     *
     * type: (str) -> SubtypeTree
     *
     * Given an encoded aux_data type_name, generate its parse tree.
     *
     * >>> _parse_type('foo')
     * ('foo', ())
     *
     *  >>> _parse_type('foo<bar>')
     *  ('foo', (('bar',()),))
     *
     *  >>> _parse_type('foo<bar<baz>>')
     *  ('foo', (('bar', (('baz', ()),)),))
     *
     *  {@param} typeName: The type name to parse into a "SubtypeTree".
     *
     */
    public TwoTuple<String, Object> parseType(String typeName) {
        // Break the type string into a list of tokens
        List<String> tokens = new LinkedList<String>();
        Pattern pattern = Pattern.compile("[^<>,]+|<|>|,");
        Matcher matcher = pattern.matcher(typeName);
        while (matcher.find())
            tokens.add(matcher.group());
        List<TwoTuple<String, Object>> result =
            parse(tokens, null).getSubtree();
        if (result.size() != 1) {
            System.err.println("parseType error: error! 5");
            return null;
        }
        // The list has one tuple holding the parsed tree
        return result.get(0);
    }

    /**
     * Decode the data in 'byteBuffer' given a parsed type tree.
     *
     * @param byteBuffer   The binary stream to read bytes from.
     * @param parseTree    The parsed type of the object encoded by
     * 'byteBuffer'.
     * @return             Decoded AuxData according to parsed type tree.
     */
    public static Object decodeTree(Serialization byteBuffer,
                                    TwoTuple<String, Object> parseTree) {
        String typeName = parseTree.getFirst();
        List<TwoTuple<String, Object>> subtypes =
            (List<TwoTuple<String, Object>>)parseTree.getSecond();
        Codec codec = getCodec(typeName);
        if (codec == null) {
            throw new UnknownCodecException(typeName);
        }
        return codec.decode(byteBuffer, subtypes);
    }

    /**
     * Encode the {@link AuxData} given a parsed type tree.
     * @param outstream  A binary stream to write bytes to.
     * @param val        The {@link AuxData} to encode.
     * @param parseTree  The parsed type to encode with.
     */
    public static void encodeTree(StreamSerialization outstream, Object val,
                                  TwoTuple<String, Object> parseTree) {
        String typeName = parseTree.getFirst();
        List<TwoTuple<String, Object>> subtypes =
            (List<TwoTuple<String, Object>>)parseTree.getSecond();
        Codec codec = getCodec(typeName);
        if (codec == null) {
            throw new UnknownCodecException(typeName);
        }
        codec.encode(outstream, val, subtypes);
    }

    /**
     * Decode auxdata from a byte array using a typeName
     *
     * @param rawBytes  The bytes from which to read the encoded value.
     * @param typeName  The type name of the object encoded by 'rawNytes'.
     * @return          The object encoded by 'rawBytes'.
     */
    public Object decode(byte[] rawBytes, String typeName) {
        Object result;
        try {
            TwoTuple<String, Object> parseTree = parseType(typeName);
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
    public byte[] encode(Object val, String typeName) {
        if (val instanceof UnknownData)
            return ((UnknownData)val).bytes;
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        StreamSerialization outstream = new StreamSerialization(baos);
        TwoTuple<String, Object> parseTree = parseType(typeName);
        try {
            encodeTree(outstream, val, parseTree);
        } catch (UnknownCodecException ex) {
            throw new EncodeException("unknown codec: " + parseTree.getFirst());
        }
        return baos.toByteArray();
    }
}
