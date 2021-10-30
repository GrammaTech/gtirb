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

import java.util.UUID;

import com.grammatech.gtirb.proto.CFGOuterClass;

/**
 * A CFG Edge represents an edge in the interprocedural control flow graph
 * (CFG).
 */
public class Edge {

    /**
     * Indicates the type of control flow transfer indicated by this edge.
     */
    public enum EdgeType {
        Branch,
        Call,
        Fallthrough,
        Return,
        Syscall,
        Sysret,
        Unlabelled
    }

    private UUID sourceUuid;
    private UUID targetUuid;
    private EdgeType edgeType;
    private boolean edgeLabelConditional;
    private boolean edgeLabelDirect;

    /**
     * Class constructor for an Edge from a protobuf edge.
     * @param  protoEdge  The edge as serialized into a protocol buffer.
     */
    public Edge(CFGOuterClass.Edge protoEdge) {
        this.setSourceUuid(Util.byteStringToUuid(protoEdge.getSourceUuid()));
        this.setTargetUuid(Util.byteStringToUuid(protoEdge.getTargetUuid()));
        if (protoEdge.hasLabel()) {
            CFGOuterClass.EdgeLabel protoEdgeLabel = protoEdge.getLabel();
            this.edgeType = EdgeType.values()[protoEdgeLabel.getTypeValue()];
            this.setEdgeLabelConditional(protoEdgeLabel.getConditional());
            this.setEdgeLabelDirect(protoEdgeLabel.getDirect());
        } else {
            this.edgeType = EdgeType.Unlabelled;
        }
    }

    /**
     * Class constructor for an Edge.
     * @param  sourceUuid  UUID of the source node.
     * @param  targetUuid  UUID of the target node.
     * @param  edgeType  The {@link EdgeType}.
     * @param  isConditional  True if the edge id conditional.
     * @param  isDirect  True if the edge is direct.
     */
    public Edge(UUID sourceUuid, UUID targetUuid, EdgeType edgeType,
                boolean isConditional, boolean isDirect) {
        this.setSourceUuid(sourceUuid);
        this.setTargetUuid(targetUuid);
        this.edgeType = edgeType;
        this.edgeLabelConditional = isConditional;
        this.edgeLabelDirect = isDirect;
    }

    /**
     * Get the source node of an {@link Edge}.
     *
     * @return  The edge source node (UUID).
     */
    public UUID getSourceUuid() { return this.sourceUuid; }

    /**
     * Set the source node of an {@link Edge}.
     *
     * @param sourceUuid  The edge source node (UUID).
     */
    public void setSourceUuid(UUID sourceUuid) { this.sourceUuid = sourceUuid; }

    /**
     * Get the target node of an {@link Edge}.
     *
     * @return  The edge target node (UUID).
     */
    public UUID getTargetUuid() { return this.targetUuid; }

    /**
     * Set the target node of an {@link Edge}.
     *
     * @param targetUuid  The edge target node (UUID).
     */
    public void setTargetUuid(UUID targetUuid) { this.targetUuid = targetUuid; }

    /**
     * Get the {@link EdgeType} of an {@link Edge}.
     *
     * @return  The edge type.
     */
    public EdgeType getEdgeType() { return this.edgeType; }

    /**
     * Set the {@link EdgeType} of an {@link Edge}.
     *
     * @param edgeType  The edge type.
     */
    public void setEdgeType(EdgeType edgeType) { this.edgeType = edgeType; }

    /**
     * Whether an {@link Edge}.
     *
     * @return  True if the edge is conditional.
     */
    public boolean isConditional() { return this.edgeLabelConditional; }

    /**
     * Set whether an {@link Edge} is conditional or not.
     *
     * @param conditional  True if the edge is conditional.
     */
    public void setEdgeLabelConditional(boolean conditional) {
        this.edgeLabelConditional = conditional;
    }

    /**
     * Whether an {@link Edge}.
     *
     * @return  True if the edge is direct.
     */
    public boolean isDirect() { return this.edgeLabelDirect; }

    /**
     * Set the name of a {@link Edge}.
     *
     * @param direct  True if the edge is direct.
     */
    public void setEdgeLabelDirect(boolean direct) {
        this.edgeLabelDirect = direct;
    }

    /**
     * De-serialize a {@link Edge} from a protobuf .
     *
     * @param  protoEdge  The edge as serialized into a protocol buffer.
     * @return An initialized Edge.
     */
    public static Edge fromProtobuf(CFGOuterClass.Edge protoEdge) {
        return new Edge(protoEdge);
    }

    /**
     * Serialize this {@link Edge} into a protobuf.
     *
     * @return edge protocol buffer.
     */
    public CFGOuterClass.Edge.Builder toProtobuf() {
        CFGOuterClass.Edge.Builder protoEdge = CFGOuterClass.Edge.newBuilder();
        if (this.edgeType == EdgeType.Unlabelled) {
            protoEdge.clearLabel();
        } else {
            CFGOuterClass.EdgeLabel.Builder protoEdgeLabel =
                CFGOuterClass.EdgeLabel.newBuilder();
            protoEdgeLabel.setTypeValue(this.edgeType.ordinal());
            protoEdgeLabel.setConditional(this.edgeLabelConditional);
            protoEdgeLabel.setDirect(this.edgeLabelDirect);
            protoEdge.setLabel(protoEdgeLabel);
        }
        protoEdge.setSourceUuid(Util.uuidToByteString(this.sourceUuid));
        protoEdge.setTargetUuid(Util.uuidToByteString(this.targetUuid));
        return protoEdge;
    }
}
