import Block_pb2
import ProxyBlock_pb2

from gtirb.node import Node


class Block(Node):
    """
    A basic block.
    """

    def __init__(self, address=0, decode_mode=0, size=0, uuid=None):
        super().__init__(uuid)
        self.address = address
        self.size = size
        self.decode_mode = decode_mode

    def _to_protobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = Block_pb2.Block()
        ret.uuid = self.uuid.bytes
        ret.address = self.address
        ret.size = self.size
        ret.decode_mode = self.decode_mode
        return ret

    @classmethod
    def _decode_protobuf(cls, proto_block, uuid):
        return cls(proto_block.address,
                   proto_block.decode_mode,
                   proto_block.size,
                   uuid)


class ProxyBlock(Node):
    """
    A placeholder to serve as the endpoint of a CFG edge.

    A ProxyBlock exists in the CFG so that edges to or from another
    node may be constructed. For example, a call to a function in
    another module may be represented by an edge that originates at
    the calling block and targets a proxy. Another example would be an
    edge to represent an indirect jump whose target is not known.

    ProxyBlocks do not represent any instructions and so have neither
    an address nor a size.
    """

    def _to_protobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = ProxyBlock_pb2.ProxyBlock()
        ret.uuid = self.uuid.bytes
        return ret

    @classmethod
    def _decode_protobuf(cls, proto_proxy, uuid):
        return cls(uuid)
