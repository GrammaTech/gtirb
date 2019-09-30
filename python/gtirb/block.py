import Block_pb2
import ProxyBlock_pb2

from .node import Node


class Block(Node):
    """A basic block in the binary.

    :ivar address: The starting address of the block.
    :ivar size: The length of the block in bytes.
    :ivar decode_mode: The decode mode of the block.
    """

    def __init__(self, address, size, *, decode_mode=0, uuid=None):
        """
        :param address: The starting address of the block.
        :param size: The length of the block in bytes.
        :param decode_mode: The decode mode of the block. Defaults to 0.
        :param uuid: The UUID of this Node,
            or None if a new UUID needs generated via :func:`uuid.uuid4`.
            Defaults to None.
        """

        super().__init__(uuid)
        self.address = address
        self.size = size
        self.decode_mode = decode_mode

    @classmethod
    def _decode_protobuf(cls, proto_block, uuid):
        return cls(
            address=proto_block.address,
            decode_mode=proto_block.decode_mode,
            size=proto_block.size,
            uuid=uuid,
        )

    def _to_protobuf(self):
        proto_block = Block_pb2.Block()
        proto_block.uuid = self.uuid.bytes
        proto_block.address = self.address
        proto_block.size = self.size
        proto_block.decode_mode = self.decode_mode
        return proto_block

    def deep_eq(self, other):
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, Block):
            return False
        return (
            self.uuid == other.uuid
            and self.address == other.address
            and self.size == other.size
            and self.decode_mode == other.decode_mode
        )

    def __repr__(self):
        return (
            "Block("
            "uuid={uuid!r}, "
            "address={address:#x}, "
            "size={size}, "
            "decode_mode={decode_mode}, "
            ")".format(**self.__dict__)
        )


class ProxyBlock(Node):
    """A placeholder to serve as the endpoint of a CFG edge.

    A ProxyBlock exists in the CFG so that edges to or from another
    node may be constructed. For example, a call to a function in
    another module may be represented by an edge that originates at
    the calling block and targets a proxy. Another example would be an
    edge to represent an indirect jump whose target is not known.

    ProxyBlocks do not represent any instructions and so have neither
    an address nor a size.
    """

    @classmethod
    def _decode_protobuf(cls, proto_proxy, uuid):
        return cls(uuid)

    def _to_protobuf(self):
        proto_proxyblock = ProxyBlock_pb2.ProxyBlock()
        proto_proxyblock.uuid = self.uuid.bytes
        return proto_proxyblock

    def deep_eq(self, other):
        # Do not move __eq__. See docstring for Node.deep_eq for more info.
        if not isinstance(other, ProxyBlock):
            return False
        return self.uuid == other.uuid

    def __repr__(self):
        return "ProxyBlock(" "uuid={uuid!r}, " ")".format(**self.__dict__)
