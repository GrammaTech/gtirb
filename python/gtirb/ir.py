"""IR Module
Sample usage.

Opening a GTIR file and loading it into an IR instance

    ir = IR.load_protobuf('filename.gtir')

Writing back the ir instance as a protobuf file

    IR.save_protobuf('filename.gtir')

"""

from uuid import UUID, uuid4

import IR_pb2

from gtirb.auxdata import AuxData
from gtirb.auxdatacontainer import AuxDataContainer
from gtirb.module import Module


class IR(AuxDataContainer):
    """
    A complete internal representation consisting of multiple Modules.
    """

    def __init__(self, uuid=None, modules=list(),
                 aux_data=dict(), uuid_cache=None):
        """IR constructor. Can be used to construct an empty IR instance

        :param uuid: UUID. Creates a new instance if None
        :param modules: List of modules
        :param aux_data: auxilary data hanging off the IR
        :param uuid_cache: uuid_cache
        :returns: IR
        :rtype: IR
        """
        if uuid is None:
            uuid = uuid4()
        self.uuid = uuid
        if uuid_cache is not None:
            uuid_cache[uuid] = self
        modules = list(modules)
        self.modules = modules
        super().__init__(aux_data)

    def _to_protobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = IR_pb2.IR()
        ret.uuid = self.uuid.bytes
        ret.modules.extend(m._to_protobuf() for m in self.modules)
        ret.aux_data_container.CopyFrom(super()._to_protobuf())
        return ret

    @classmethod
    def _from_protobuf(cls, protobuf_ir, uuid_cache=None):
        """Load pygtirb class from protobuf object

        :param cls: this class
        :param protobuf_ir: the protobuf IR object
        :returns: the pygtirb IR object
        :rtype: IR

        """
        uuid = UUID(bytes=protobuf_ir.uuid)
        uuid_cache = dict()
        modules = [Module._from_protobuf(m, uuid_cache)
                   for m in protobuf_ir.modules]
        aux_data = {
            key: AuxData._from_protobuf(val)
            for key, val in protobuf_ir.aux_data_container.aux_data.items()
        }
        ir = cls(uuid, modules, aux_data, uuid_cache)
        return ir

    @staticmethod
    def load_protobuf_file(protobuf_file):
        """Load IR from protobuf file object

        :param protobuf_file: The protobuf file object
        :returns: GTIR
        :rtype: IR

        """
        ir = IR_pb2.IR()
        ir.ParseFromString(protobuf_file.read())
        return IR._from_protobuf(ir)

    @staticmethod
    def load_protobuf(file_name):
        """Load IR from protobuf file at path.

        :param file_name: The given protobuf GTIR file path
        :returns: GTIR
        :rtype: IR
        """
        with open(file_name, 'rb') as f:
            return IR.load_protobuf_file(f)

    def save_protobuf_file(self, protobuf_file):
        """Save IR to protobuf file object

        :param protobuf_file: The protobuf file object
        """
        protobuf_file.write(self._to_protobuf().SerializeToString())

    def save_protobuf(self, file_name):
        """Save IR to protobuf file at path.

        :param file_name: The given protobuf GTIR file path
        """
        with open(file_name, 'wb') as f:
            self.save_protobuf_file(f)
