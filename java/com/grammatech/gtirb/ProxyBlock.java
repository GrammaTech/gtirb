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

import com.grammatech.gtirb.proto.ProxyBlockOuterClass;

/**
 * ProxyBlock is a placeholder that serves as the end point (source or target)
 * of a CfgEdge.
 */
public class ProxyBlock extends Node {

    private ProxyBlockOuterClass.ProxyBlock protoProxyBlock;
    private Module module;

    /**
     * Class constructor for a ProxyBlock from a protobuf section.
     * @param  protoProxyBlock  The ProxyBlock as serialized into a protocol
     * buffer.
     * @param  module        The Module that owns this Section.
     */
    private ProxyBlock(ProxyBlockOuterClass.ProxyBlock protoProxyBlock,
                       Module module) {
        super(Util.byteStringToUuid(protoProxyBlock.getUuid()));
        this.module = module;
        this.protoProxyBlock = protoProxyBlock;
    }

    /**
     * Class Constructor.
     */
    public ProxyBlock(Module module) {
        super();
        this.module = module;
        this.protoProxyBlock = null;
    }

    /**
     * Get the original protobuf of this {@link ProxyBlock}.
     *
     * @return The protobuf the proxy block was imported from, or
     * null if it was not imported from a protobuf.
     */
    public ProxyBlockOuterClass.ProxyBlock getProtoProxyBlock() {
        return this.protoProxyBlock;
    }

    /**
     * Get the Module this ProxyBlock belongs to.
     *
     * @return  The Module that this ProxyBlock belongs to, or null if it
     * does not belong to any Module.
     */
    public Module getModule() { return this.module; }

    /**
     * De-serialize a {@link ProxyBlock} from a protobuf .
     *
     * @param  protoProxyBlock  The ProxyBlock as serialized into a protocol
     * buffer.
     * @param  module        The Module that owns this Section.
     * @return An initialized proxy block.
     */
    static ProxyBlock
    fromProtobuf(ProxyBlockOuterClass.ProxyBlock protoProxyBlock,
                 Module module) {
        return new ProxyBlock(protoProxyBlock, module);
    }

    /**
     * Serialize this ProxyBlock into a protobuf .
     *
     * @return A protocol buffer containing this ProxyBlock.
     */
    ProxyBlockOuterClass.ProxyBlock.Builder toProtobuf() {
        ProxyBlockOuterClass.ProxyBlock.Builder protoProxyBlock =
            ProxyBlockOuterClass.ProxyBlock.newBuilder();
        protoProxyBlock.setUuid(Util.uuidToByteString(this.getUuid()));
        return protoProxyBlock;
    }
}
