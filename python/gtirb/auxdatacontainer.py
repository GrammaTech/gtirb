import AuxDataContainer_pb2

from gtirb.auxdata import AuxData


class AuxDataContainer:
    """Holds AuxData tables, base class for IR and Module
    """

    def __init__(self, aux_data=None):
        """Constructor
        :param aux_data: dict(str, AuxData), optional dict mapping
            type names to AuxData objects
        :returns: AuxDataContainer
        :rtype: AuxDataContainer
        """
        if aux_data is None:
            self.aux_data = dict()
        else:
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
    def _from_protobuf(cls, aux_data_container, uuid_cache=None):
        """Load pygtirb object from protobuf object

        :param cls: this class
        :param aux_data_container: protobuf object
        :param uuid_cache: uuid cache
        :returns: pygtirb object
        :rtype: AuxDataContainer

        """
        return cls(
            (key, AuxData._from_protobuf(val))
            for key, val in aux_data_container.aux_data.items()
        )
