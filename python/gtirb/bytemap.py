import ByteMap_pb2


class ByteMap:
    """
    Holds the bytes of the loaded image of the binary.
    """

    def __init__(self, regions=None):
        if regions is None:
            regions = []
        self.regions = regions

    def _to_protobuf(self):
        """
        Returns protobuf representation of the object

        :returns: protobuf representation of the object
        :rtype: protobuf object

        """

        ret = ByteMap_pb2.ByteMap()

        def region_to_protobuf(region):
            reg = ByteMap_pb2.Region()
            reg.address, reg.data = region
            return reg

        ret.regions.extend(region_to_protobuf(r) for r in self.regions)
        return ret

    @classmethod
    def _from_protobuf(cls, byte_map, uuid_cache=None):
        """
        Load this cls from protobuf object
        """
        return cls([(region.address, region.data)
                    for region in byte_map.regions])

    def add_region(self, addr, data):
        """ Add region to this ByteMap """
        self.regions.append((addr, data))
