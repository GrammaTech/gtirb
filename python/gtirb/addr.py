class Addr:
    """A class for holding addresses. This is defined because codecs are
    selected based on the class name during encoding."""
    def __init__(self, address=None):
        self.address = address

    def __eq__(self, other):
        return isinstance(other, type(self)) and \
            self.address == other.address

    def __hash__(self):
        return hash(self.address)
