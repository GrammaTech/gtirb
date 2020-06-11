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

import com.grammatech.gtirb.proto.ProxyBlockOuterClass;

public class ProxyBlock extends Node {

    private ProxyBlockOuterClass.ProxyBlock protoProxyBlock;

    public ProxyBlock(com.grammatech.gtirb.proto.ProxyBlockOuterClass
                          .ProxyBlock protoProxyBlock) {
        this.protoProxyBlock = protoProxyBlock;
        super.setUuid(Util.byteStringToUuid(protoProxyBlock.getUuid()));
    }

    public ProxyBlockOuterClass.ProxyBlock getProtoProxyBlock() {
        return this.protoProxyBlock;
    }
}
