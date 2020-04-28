"""General utilities usable by any other GTIRB submoudle."""

import itertools
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

    # The version of typing.py which comes with python 3.5.2 doesn't provide
    # definitions for append or remove on MutableList, so we have to do it
    # ourselves.
    def append(self, v):
        self.insert(len(self), v)

    def remove(self, v):
        del self[self._data.index(v)]

    # extend is not in every version of Python 3, so list wrapper adds it here
    # itself.
    def extend(self, other):
        for v in other:
            self.append(v)

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

    # The version of typing.py which comes with python 3.5.2 doesn't provide
    # definitions for __or__ or clear on MutableSet, so we have to do it
    # ourselves.
    def __or__(self, other):
        return self._data | other

    def clear(self):
        self._data.clear()

    # For whatever reason, update isn't included as part of abc.MutableSet.
    def update(self, *others):
        for other in others:
            for v in other:
                self.add(v)

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


def nodes_on(
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


def symbolic_expressions_at(
    nodes,  # type: typing.Iterable
    addrs,  # type: typing.Union[int, range]
):
    # type: (...) -> typing.Iterable[typing.Tuple]
    return itertools.chain.from_iterable(
        node.symbolic_expressions_at(addrs) for node in nodes
    )
