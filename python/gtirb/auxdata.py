from io import BytesIO

import AuxData_pb2
import AuxDataContainer_pb2

from .node import Node
from .serialization import Serialization


class AuxData:
    """Types and operations for auxiliary data.  AuxData objects can be
    attached to the IR or individual Modules to store additional
    client-specific data in a portable way.

    Attributes:
        serializer: A Serialization object used for encoding/decoding aux data
        data: data associated with this aux data
        type_name: optional string describing this aux data

    """
    serializer = Serialization()

    def __init__(self, data, type_name=None):
        self.data = data
        self.type_name = type_name

    @classmethod
    def _from_protobuf(cls, aux_data):
        """Deserialize AuxData from Protobuf.

        Parameters:
            cls: a Python AuxData instance
            aux_data: the Protobuf AuxData object

        """
        data = AuxData.serializer.decode(BytesIO(aux_data.data),
                                         aux_data.type_name)
        return cls(data=data, type_name=aux_data.type_name)

    def _to_protobuf(self):
        """Get a Protobuf representation of the AuxData"""

        proto_auxdata = AuxData_pb2.AuxData()
        out_bytes_array = BytesIO()
        proto_auxdata.type_name = \
            AuxData.serializer.encode(out_bytes_array,
                                      self.data,
                                      self.type_name)
        proto_auxdata.data = out_bytes_array.getvalue()
        return proto_auxdata


class AuxDataContainer(Node):
    """Holds AuxData tables, base class for IR and Module

    Attributes:
        aux_data: dict(str, AuxData), optional dict mapping type names to
            AuxData objects
        uuid: the UUID of this Node

    """
    def __init__(self, aux_data=dict(), uuid=None):
        super().__init__(uuid)
        self.aux_data = dict(aux_data)

    @classmethod
    def _decode_protobuf(cls, proto_container, uuid):
        aux_data = ((key, AuxData.from_protobuf(val))
                    for key, val in proto_container.aux_data.items())
        return cls(aux_data, uuid)

    def _to_protobuf(self):
        proto_auxdatacontainer = AuxDataContainer_pb2.AuxDataContainer()
        for k, v in self.aux_data.items():
            proto_auxdatacontainer.aux_data[k].CopyFrom(v._to_protobuf())
        return proto_auxdatacontainer
