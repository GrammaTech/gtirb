"""IR Module
Sample usage.

Opening a GTIR file and loading it into an IR instance

    ir = IR.load_protobuf('filename.gtir')

Writing back the ir instance as a protobuf file

    IR.save_protobuf('filename.gtir')

"""

import IR_pb2

from .auxdata import AuxData
from .auxdatacontainer import AuxDataContainer
from .module import Module


class IR(AuxDataContainer):
    """
    A complete internal representation consisting of multiple Modules.
    """

    def __init__(self, modules=list(), aux_data=dict(), uuid=None):
        """IR constructor. Can be used to construct an empty IR instance

        :param uuid: UUID. Creates a new instance if None
        :param modules: List of modules
        :param aux_data: auxilary data hanging off the IR
        :returns: IR
        :rtype: IR
        """
        super().__init__(aux_data, uuid)
        self.modules = list(modules)

    @classmethod
    def _decode_protobuf(cls, proto_ir, uuid):
        modules = [Module.from_protobuf(m)
                   for m in proto_ir.modules]
        aux_data = {
            key: AuxData.from_protobuf(val)
            for key, val in proto_ir.aux_data_container.aux_data.items()
        }
        return cls(modules, aux_data, uuid)

    @staticmethod
    def load_protobuf_file(protobuf_file):
        """Load IR from protobuf file object

        :param protobuf_file: The protobuf file object
        :returns: GTIR
        :rtype: IR

        """
        ir = IR_pb2.IR()
        ir.ParseFromString(protobuf_file.read())
        return IR.from_protobuf(ir)

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
        protobuf_file.write(self.to_protobuf().SerializeToString())

    def save_protobuf(self, file_name):
        """Save IR to protobuf file at path.

        :param file_name: The given protobuf GTIR file path
        """
        with open(file_name, 'wb') as f:
            self.save_protobuf_file(f)

    def to_protobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        proto_ir = IR_pb2.IR()
        proto_ir.uuid = self.uuid.bytes
        proto_ir.modules.extend(m.to_protobuf() for m in self.modules)
        proto_ir.aux_data_container.CopyFrom(super().to_protobuf())
        return proto_ir
