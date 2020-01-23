/** */
package com.grammatech.gtirb;

import java.util.UUID;

public class ProxyBlock extends Node {

    public ProxyBlock(com.grammatech.gtirb.proto.ProxyBlockOuterClass
                          .ProxyBlock protoProxyBlock) {
        UUID uuid = Util.byteStringToUuid(protoProxyBlock.getUuid());
        super.setUuid(uuid);
    }
}
