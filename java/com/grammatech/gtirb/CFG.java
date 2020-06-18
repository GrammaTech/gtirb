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

import com.grammatech.gtirb.proto.CFGOuterClass;
import java.util.ArrayList;

public class CFG {

    private ArrayList<Edge> edgeList;
    private ArrayList<byte[]> verticeList;

    // TODO the saving of protoCfg and using it for mergeFrom
    // should be replaced with deriving CFG from listing
    private CFGOuterClass.CFG protoCfg;

    public CFG(com.grammatech.gtirb.proto.CFGOuterClass.CFG protoCfg) {
        this.edgeList = new ArrayList<Edge>();
        this.verticeList = new ArrayList<byte[]>();
        this.protoCfg = protoCfg;

        for (com.grammatech.gtirb.proto.CFGOuterClass.Edge protoEdge :
             protoCfg.getEdgesList()) {
            Edge edge = new Edge(protoEdge);
            edgeList.add(edge);
        }
        for (com.google.protobuf.ByteString byteString :
             protoCfg.getVerticesList()) {
            byte[] vertice = byteString.toByteArray();
            verticeList.add(vertice);
        }
    }

    public ArrayList<Edge> getEdgeList() { return this.edgeList; }

    public ArrayList<byte[]> getVerticeList() { return this.verticeList; }

    public CFGOuterClass.CFG.Builder buildCFG() {
        CFGOuterClass.CFG.Builder newCFG = CFGOuterClass.CFG.newBuilder();
        newCFG.mergeFrom(this.protoCfg);
        return newCFG;
    }
}
