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

    def __init__(self, data, type_name):
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

        out_bytes_array = BytesIO()
        AuxData.serializer.encode(out_bytes_array, self.data, self.type_name)
        proto_auxdata = AuxData_pb2.AuxData()
        proto_auxdata.type_name = self.type_name
        proto_auxdata.data = out_bytes_array.getvalue()
        return proto_auxdata

    def __repr__(self):
        return "gtirb.AuxData(type_name=%r, data=%r)"\
               % (self.type_name, self.data)


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

    def deep_eq(self, other):
        """Compare structural equality

        This method should be used only when deep structural equality checks
        are actually needed, and not for all equality checks. Typically the
        default implmentation of __eq__, which checks pointer equality, is
        enough: UUID checks are part of deep equality checks, and generating a
        new Node generates a unique UUID.

        Note that because AuxData can store any type of data, it is not deeply
        checked. This method only checks that two AuxDataContainers contain the
        same keys.

        """
        if not isinstance(other, AuxDataContainer):
            return False
        if self.uuid != other.uuid \
           or self.aux_data.keys() != other.aux_data.keys():
            return False
        return True
