import AuxDataContainer_pb2

from gtirb.auxdata import AuxData
from gtirb.node import Node


class AuxDataContainer(Node):
    """Holds AuxData tables, base class for IR and Module
    """

    def __init__(self, aux_data=dict(), uuid=None):
        """Constructor
        :param aux_data: dict(str, AuxData), optional dict mapping
            type names to AuxData objects
        :returns: AuxDataContainer
        :rtype: AuxDataContainer
        """
        super().__init__(uuid)
        self.aux_data = dict(aux_data)

    def _to_protobuf(self):
        """Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """
        ret = AuxDataContainer_pb2.AuxDataContainer()
        for k, v in self.aux_data.items():
            ret.aux_data[k].CopyFrom(v._to_protobuf())
        return ret

    @classmethod
    def _decode_protobuf(cls, proto_container, uuid):
        """Load pygtirb object from protobuf object

        :param cls: this class
        :param aux_data_container: protobuf object
        :param uuid_cache: uuid cache
        :returns: pygtirb object
        :rtype: AuxDataContainer

        """
        aux_data = ((key, AuxData.from_protobuf(val))
                     for key, val in proto_container.aux_data.items())
        return cls(aux_data, uuid)
