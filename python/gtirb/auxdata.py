from io import BytesIO

import AuxData_pb2

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
        """Constructor
        :param data: aux data
        :param type_name: `str`, optional. Type of the data.
        """
        self.data = data
        self.type_name = type_name

    @classmethod
    def _from_protobuf(cls, aux_data):
        """
        Load pygtirb class from protobuf class
        """
        data = AuxData.serializer.decode(aux_data.type_name,
                                         BytesIO(aux_data.data))
        return cls(data=data, type_name=aux_data.type_name)

    def _to_protobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        proto_auxdata = AuxData_pb2.AuxData()
        out_bytes_array = BytesIO()
        proto_auxdata.type_name = \
            AuxData.serializer.encode(out_bytes_array,
                                      self.data,
                                      type_name_hint=self.type_name)
        out_bytes_array.seek(0)
        proto_auxdata.data = out_bytes_array.read()
        return proto_auxdata
