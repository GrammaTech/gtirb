import Block_pb2
import ProxyBlock_pb2

from .node import Node


class Block(Node):
    """Basic block

    Attributes:
        address: the starting address of the basic block
        size: the length of the basic block
        decode_mode: the decode mode of the block
        uuid: UUID of this Node

    """
    def __init__(self, address, decode_mode, size, uuid):
        super().__init__(uuid)
        self.address = address
        self.size = size
        self.decode_mode = decode_mode

    @classmethod
    def _decode_protobuf(cls, proto_block, uuid):
        return cls(proto_block.address,
                   proto_block.decode_mode,
                   proto_block.size,
                   uuid)

    def _to_protobuf(self):
        proto_block = Block_pb2.Block()
        proto_block.uuid = self.uuid.bytes
        proto_block.address = self.address
        proto_block.size = self.size
        proto_block.decode_mode = self.decode_mode
        return proto_block


class ProxyBlock(Node):
    """A placeholder to serve as the endpoint of a CFG edge.

    A ProxyBlock exists in the CFG so that edges to or from another
    node may be constructed. For example, a call to a function in
    another module may be represented by an edge that originates at
    the calling block and targets a proxy. Another example would be an
    edge to represent an indirect jump whose target is not known.

    ProxyBlocks do not represent any instructions and so have neither
    an address nor a size.

    Attributes:
        uuid: the UUID of this Node

    """
    @classmethod
    def _decode_protobuf(cls, proto_proxy, uuid):
        return cls(uuid)

    def _to_protobuf(self):
        proto_proxyblock = ProxyBlock_pb2.ProxyBlock()
        proto_proxyblock.uuid = self.uuid.bytes
        return proto_proxyblock
