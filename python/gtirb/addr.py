class Addr:
    """Holds addresses

    Defined because codecs are selected based on the class name during
    serialization.

    Attributes:
        address: unsigned integer address

    """
    def __init__(self, address):
        self.address = address
