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

import com.google.protobuf.ByteString;
import com.grammatech.gtirb.proto.CFGOuterClass;
import java.util.ArrayList;
import java.util.List;

/**
 * A CFG represents the interprocedural control flow graph.
 */
public class CFG {

    private List<Edge> edgeList;
    private List<byte[]> verticeList;

    /**
     * Class constructor for a {@link CFG} from a protobuf CFG.
     * @param  protoCfg  The CFG as serialized into a protocol buffer.
     */
    public CFG(CFGOuterClass.CFG protoCfg) {
        this.edgeList = new ArrayList<Edge>();
        this.verticeList = new ArrayList<byte[]>();
        for (CFGOuterClass.Edge protoEdge : protoCfg.getEdgesList()) {
            Edge edge = new Edge(protoEdge);
            edgeList.add(edge);
        }
        for (com.google.protobuf.ByteString byteString :
             protoCfg.getVerticesList()) {
            byte[] vertice = byteString.toByteArray();
            verticeList.add(vertice);
        }
    }

    /**
     * Class constructor for a {@link CFG}.
     * @param  edgeList  The edges belonging to to this CFG.
     * @param  verticeList  The vertices belonging to this CFG.
     */
    public CFG(List<Edge> edgeList, List<byte[]> verticeList) {
        this.edgeList = edgeList;
        this.verticeList = verticeList;
    }

    /**
     * Get the {@link Edge} list of a {@link CFG}.
     *
     * @return  The edge list.
     */
    public List<Edge> getEdgeList() { return this.edgeList; }

    /**
     * Set the {@link Edge} list of a {@link CFG}.
     *
     * @param edgeList  The edge list.
     */
    public void setEdgeList(List<Edge> edgeList) { this.edgeList = edgeList; }

    /**
     * Get the vertice list of a {@link CFG}.
     *
     * @return  The vertice list.
     */
    public List<byte[]> getVerticeList() { return this.verticeList; }

    /**
     * Set the vertice list of a {@link CFG}.
     *
     * @param verticeList  The vertice list.
     */
    public void setVerticeList(List<byte[]> verticeList) {
        this.verticeList = verticeList;
    }

    // DEPRECATED
    //    public CFGOuterClass.CFG.Builder buildCFG() {
    //        CFGOuterClass.CFG.Builder newCFG = CFGOuterClass.CFG.newBuilder();
    //        newCFG.mergeFrom(this.protoCfg);
    //        return newCFG;
    //    }

    /**
     * De-serialize a {@link CFG} from a protobuf .
     *
     * @param  protoCFG  The CFG as serialized into a protocol buffer.
     * @return An initialized CFG.
     */
    public static CFG fromProtobuf(CFGOuterClass.CFG protoCFG) {
        return new CFG(protoCFG);
    }

    /**
     * Serialize this {@link CFG} into a protobuf.
     *
     * @return edge protocol buffer.
     */
    public CFGOuterClass.CFG.Builder toProtobuf() {
        CFGOuterClass.CFG.Builder protoCfg = CFGOuterClass.CFG.newBuilder();

        for (byte[] vertice : this.verticeList)
            protoCfg.addVertices(ByteString.copyFrom(vertice));
        for (Edge edge : this.edgeList)
            protoCfg.addEdges(edge.toProtobuf());
        return protoCfg;
    }
}
