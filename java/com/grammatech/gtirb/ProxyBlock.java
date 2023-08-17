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

import com.grammatech.gtirb.proto.ProxyBlockOuterClass;
import java.util.Optional;

/**
 * ProxyBlock is a placeholder that serves as the end point (source or target)
 * of a CfgEdge.
 */
public class ProxyBlock extends Node {

    private Optional<Module> module;

    /**
     * Class constructor for a ProxyBlock from a protobuf section.
     * @param  protoProxyBlock  The ProxyBlock as serialized into a protocol
     * buffer.
     */
    private ProxyBlock(ProxyBlockOuterClass.ProxyBlock protoProxyBlock) {
        super(Util.byteStringToUuid(protoProxyBlock.getUuid()));
        this.module = Optional.empty();
    }

    /**
     * Class Constructor.
     */
    public ProxyBlock() {
        super();
        this.module = Optional.empty();
    }

    /**
     * Get the {@link Module} this ProxyBlock belongs to.
     *
     * @return  An Optional that contains the Module this
     * proxy block belongs to, or empty if it does not belong to a Module.
     */
    public Optional<Module> getModule() { return this.module; }

    /**
     * Set the Module this ProxyBlock belongs to.
     *
     * @param  An Optional that contains the Module this
     * proxy block belongs to, or empty if it does not belong to a Module.
     */
    void setModule(Optional<Module> module) { this.module = module; }

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
        return new ProxyBlock(protoProxyBlock);
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
