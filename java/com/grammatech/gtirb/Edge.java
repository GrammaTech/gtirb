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

public class Edge {

    public enum EdgeType {
        Type_Branch(com.grammatech.gtirb.proto.CFGOuterClass.EdgeType
                        .Type_Branch_VALUE),
        Type_Call(
            com.grammatech.gtirb.proto.CFGOuterClass.EdgeType.Type_Call_VALUE),
        Type_Fallthrough(com.grammatech.gtirb.proto.CFGOuterClass.EdgeType
                             .Type_Fallthrough_VALUE),
        Type_Return(com.grammatech.gtirb.proto.CFGOuterClass.EdgeType
                        .Type_Return_VALUE),
        Type_Syscall(com.grammatech.gtirb.proto.CFGOuterClass.EdgeType
                         .Type_Syscall_VALUE),
        Type_Sysret(com.grammatech.gtirb.proto.CFGOuterClass.EdgeType
                        .Type_Sysret_VALUE);

        private int value;

        private EdgeType(int value) { this.setValue(value); }

        public int getValue() { return value; }

        public void setValue(int value) { this.value = value; }
    }

    private UUID sourceUuid;
    private UUID targetUuid;
    private EdgeType edgeType;
    private boolean edgeLabelConditional;
    private boolean edgeLabelDirect;

    public Edge(com.grammatech.gtirb.proto.CFGOuterClass.Edge protoEdge) {
        this.setSourceUuid(Util.byteStringToUuid(protoEdge.getSourceUuid()));
        this.setTargetUuid(Util.byteStringToUuid(protoEdge.getTargetUuid()));
        com.grammatech.gtirb.proto.CFGOuterClass.EdgeLabel protoEdgeLabel =
            protoEdge.getLabel();
        this.setEdgeLabelConditional(protoEdgeLabel.getConditional());
        this.setEdgeLabelDirect(protoEdgeLabel.getDirect());
        this.setEdgeType(EdgeType.values()[protoEdgeLabel.getTypeValue()]);
    }

    public UUID getSourceUuid() { return sourceUuid; }

    public void setSourceUuid(UUID sourceUuid) { this.sourceUuid = sourceUuid; }

    public UUID getTargetUuid() { return targetUuid; }

    public void setTargetUuid(UUID targetUuid) { this.targetUuid = targetUuid; }

    public EdgeType getEdgeType() { return edgeType; }

    public void setEdgeType(EdgeType edgeType) { this.edgeType = edgeType; }

    public boolean isEdgeLabelConditional() { return edgeLabelConditional; }

    public void setEdgeLabelConditional(boolean edgeLabelConditional) {
        this.edgeLabelConditional = edgeLabelConditional;
    }

    public boolean isEdgeLabelDirect() { return edgeLabelDirect; }

    public void setEdgeLabelDirect(boolean edgeLabelDirect) {
        this.edgeLabelDirect = edgeLabelDirect;
    }
}
