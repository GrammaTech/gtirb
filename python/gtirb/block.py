from uuid import UUID, uuid4

import Block_pb2
import ProxyBlock_pb2


class Block:
    """
    A basic block.
    """

    def __init__(self, uuid=None,
                 address=0, size=0, decode_mode=0, uuid_cache=None):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
        if uuid_cache is not None:
            uuid_cache[uuid] = self
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
    def _from_protobuf(cls, block, uuid_cache=None):
        """
        Load pygtirb class from protobuf class
        """
        uuid = UUID(bytes=block.uuid)
        if uuid_cache is not None and uuid in uuid_cache:
            return uuid_cache[uuid]
        return cls(uuid, block.address,
                   block.size, block.decode_mode, uuid_cache)


class ProxyBlock:
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

    def __init__(self, uuid=None, uuid_cache=None):
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
        if uuid_cache is not None:
            uuid_cache[uuid] = self

    def _to_protobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = ProxyBlock_pb2.ProxyBlock()
        ret.uuid = self.uuid.bytes
        return ret

    @classmethod
    def _from_protobuf(cls, pb, uuid_cache=None):
        """Load pygtirb object from protobuf object

        :param cls: this class
        :param pb: protobuf proxyblock object
        :param uuid_cache: uuid cache
        :returns: pygtirb proxyblock object
        :rtype: ProxyBlock

        """
        uuid = UUID(bytes=pb.uuid)
        if uuid_cache is not None and uuid in uuid_cache:
            return uuid_cache[uuid]
        return cls(uuid, uuid_cache)
