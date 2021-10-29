/*
 *  Copyright (C) 2021 GrammaTech, Inc.
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

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class AuxTypeTree {
    private final String name;
    private final List<AuxTypeTree> children;

    private AuxTypeTree(String name) {
        this.name = name;
        this.children = new ArrayList<>();
    }

    public String getName() { return this.name; }
    public List<AuxTypeTree> getChildren() { return this.children; }
    public boolean hasChild() { return !this.children.isEmpty(); }

    private static String matchName(Matcher matcher) {
        if (!matcher.find())
            throw new DecodeException("AuxData type name missing.");
        String name = matcher.group();
        if ("<>,".contains(name))
            throw new DecodeException("Unexpected '" + name +
                                      "' before AuxData type name.");
        return name;
    }

    public static AuxTypeTree parseTypeString(String typeString) {
        // Tokenize the type string
        Pattern pattern = Pattern.compile("[^<>,]+|<|>|,");
        Matcher matcher = pattern.matcher(typeString);

        LinkedList<AuxTypeTree> parseStack = new LinkedList<>();
        AuxTypeTree curNode = new AuxTypeTree(matchName(matcher));
        while (matcher.find()) {
            String token = matcher.group();
            if (token.equals("<")) {
                if (curNode.hasChild()) {
                    throw new DecodeException(
                        "Unexpected '<' in AuxData type.");
                }
                parseStack.push(curNode);
                AuxTypeTree nextNode = new AuxTypeTree(matchName(matcher));
                curNode.children.add(nextNode);
                curNode = nextNode;
            } else if (token.equals(">")) {
                if (parseStack.isEmpty()) {
                    throw new DecodeException(
                        "Unexpected '>' in AuxData type.");
                }
                curNode = parseStack.pop();
            } else if (token.equals(",")) {
                if (parseStack.isEmpty()) {
                    throw new DecodeException(
                        "Unexpected ',' in AuxData type.");
                }
                curNode = new AuxTypeTree(matchName(matcher));
                parseStack.getLast().children.add(curNode);
            } else {
                // Something like mapping<byte>foo
                throw new DecodeException(
                    "Extraneous text after '>' in AuxData type.");
            }
        }
        if (!parseStack.isEmpty()) {
            throw new DecodeException("Missing '>' in AuxData type.");
        }
        return curNode;
    }
}
