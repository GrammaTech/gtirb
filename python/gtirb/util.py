"""General utilities usable by any other GTIRB submoudle."""

import typing

K = typing.TypeVar("K")
V = typing.TypeVar("V")
T = typing.TypeVar("T")


class DictLike(typing.Generic[K, V]):
    """Any value that can be passed to the constructor of ``dict``;
    that is, a mapping or iterable yielding key-value tuples.
    """

    # NOTE: this should inherit from:
    # typing.Union[
    #     typing.Mapping[K, V],
    #     typing.Iterable[typing.Tuple[K, V]],
    # ]
    # but cannot, due to metaclass conflicts.


class ListWrapper(typing.MutableSequence[T]):
    def __init__(self, *args):
        self._data = []
        for v in list(*args):
            self.append(v)

    # begin functions for ABC
    def __getitem__(self, i):
        return self._data[i]

    def __setitem__(self, i, v):
        self._data[i] = v

    def __delitem__(self, i):
        del self._data[i]

    def __len__(self):
        return len(self._data)

    def insert(self, i, v):
        return self._data.insert(i, v)

    # end functions for ABC
    def __str__(self):
        return str(self._data)

    def __repr__(self):
        return repr(self._data)


class SetWrapper(typing.MutableSet[T]):
    def __init__(self, *args):
        self._data = set()
        for v in set(*args):
            self.add(v)

    # begin functions for ABC
    def __contains__(self, v):
        return v in self._data

    def __iter__(self):
        return iter(self._data)

    def __len__(self):
        return len(self._data)

    def add(self, v):
        return self._data.add(v)

    def discard(self, v):
        return self._data.discard(v)

    # end functions for ABC
    def __str__(self):
        return str(self._data)

    def __repr__(self):
        return repr(self._data)


class DictWrapper(typing.MutableMapping[K, V]):
    def __init__(self, *args):
        self._data = {}
        for i, v in dict(*args).items():
            self[i] = v

    # begin functions for ABC
    def __getitem__(self, i):
        return self._data[i]

    def __setitem__(self, i, v):
        self._data[i] = v

    def __delitem__(self, i):
        del self._data[i]

    def __iter__(self):
        return iter(self._data)

    def __len__(self):
        return len(self._data)

    # end functions for ABC
    def __str__(self):
        return str(self._data)

    def __repr__(self):
        return repr(self._data)


def get_desired_range(addrs):
    # type: (typing.Union[int, range]) -> range
    if isinstance(addrs, int):
        return range(addrs, addrs + 1)
    else:
        return addrs


def nodes_in(
    nodes,  # type: typing.Iterable[T]
    addrs,  # type: typing.Union[int, range]
):
    # type: (...) -> typing.Iterable[T]
    desired_range = get_desired_range(addrs)
    for node in nodes:
        node_addr = node.address
        if node_addr is not None:
            node_range = range(node_addr, node_addr + node.size)
            if range(
                max(desired_range.start, node_range.start),
                min(desired_range.stop, node_range.stop),
            ):
                yield node


def nodes_at(
    nodes,  # type: typing.Iterable[T]
    addrs,  # type: typing.Union[int, range]
):
    # type: (...) -> typing.Iterable[T]
    desired_range = get_desired_range(addrs)
    for node in nodes:
        node_addr = node.address
        if node_addr is not None and node_addr in desired_range:
            yield node
