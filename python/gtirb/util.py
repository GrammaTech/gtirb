"""General utilities usable by any other GTIRB submoudle."""

import itertools
import typing

import intervaltree
import typing_extensions

K = typing.TypeVar("K")
V = typing.TypeVar("V")
T = typing.TypeVar("T")
T_cov = typing.TypeVar("T_cov", covariant=True)
T_contra = typing.TypeVar("T_contra", contravariant=True)


DictLike = typing.Union[
    typing.Mapping[K, V], typing.Iterable[typing.Tuple[K, V]],
]
"""Any value that can be passed to the constructor of ``dict``;
that is, a mapping or iterable yielding key-value tuples.
"""


class GtirbError(Exception):
    pass


class DeserializationError(GtirbError):
    pass


class _SymbolicExpressionContainer(typing_extensions.Protocol[T_cov]):
    """A container of symbolic expressions at addresses."""

    def symbolic_expressions_at(self, addrs):
        # type: (typing.Union[int, range]) -> typing.Iterable[T_cov]
        ...


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

    def __ior__(self, other):
        for value in other:
            self.add(value)
        return self

    def pop(self):
        it = iter(self)
        # pop is documented as raising a KeyError if it's empty, not
        # StopIteration
        try:
            result = next(it)
        except StopIteration:
            raise KeyError
        self.discard(result)
        return result

    def clear(self):
        while self:
            self.pop()

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


InstanceT = typing.TypeVar("InstanceT")
AttributeT = typing.TypeVar("AttributeT")


class IndexedContainer(typing_extensions.Protocol[T_contra]):
    """Container wth an index that can be updated."""

    def _index_discard(self, instance):
        # type: (T_contra) -> None
        ...

    def _index_add(self, instance):
        # type: (T_contra) -> None
        ...


class ParentGetter(typing_extensions.Protocol[T_contra]):
    """Interface for getting an _IndexedContainer for an instance."""

    def __call__(self, instance):
        # type: (T_contra) -> typing.Optional[IndexedContainer[T_contra]]
        ...


class _IndexedAttribute(typing.Generic[AttributeT, InstanceT]):
    """
    A descriptor that will notify a parent when the value is set and can be
    otherwise used like a normal attribute.
    """

    def __init__(self, name, parent_getter):
        # type: (str, "ParentGetter[InstanceT]") -> None
        self.name = name
        self.attribute_name = "_" + name
        self.parent_getter = parent_getter

    def __get__(self, instance, owner=None):
        # type: (InstanceT, typing.Any) -> AttributeT
        return getattr(instance, self.attribute_name)

    def __set__(self, instance, value):
        # type: (InstanceT, AttributeT) -> None
        parent = self.parent_getter(instance)
        if parent:
            parent._index_discard(instance)
        setattr(instance, self.attribute_name, value)
        parent = self.parent_getter(instance)
        if parent:
            parent._index_add(instance)

    def __delete__(self, instance):
        raise AttributeError("can't delete attribute %s" % (self.name))

    def __set_name__(self, owner, name):
        # This is only invoked in Python 3.6+. Once GTIRB has that as a
        # minimum, the name paramter can be removed from the initializer and
        # taken from this instead.
        pass


def get_desired_range(addrs):
    # type: (typing.Union[int, range]) -> range
    if isinstance(addrs, int):
        return range(addrs, addrs + 1)
    else:
        return addrs


class AddrRange(typing_extensions.Protocol):
    """An object spanning a range of addresses."""

    # Protocol field types mut match exactly, but properties are alloaed to
    # return subtypes. This means that a class whose address or size is an int
    # will match Optional[int] properties, but not Optional[int] fields.

    @property
    def address(self):
        # type: () -> typing.Optional[int]
        ...

    @property
    def size(self):
        # type: () -> typing.Optional[int]
        ...


# Need a TypeVar bounded by the protocol so that nodes_on callers will get
# back the actual node type, not an AddrRange.
AddrRangeT = typing.TypeVar("AddrRangeT", bound=AddrRange)


def nodes_on(
    nodes,  # type: typing.Iterable[AddrRangeT]
    addrs,  # type: typing.Union[int, range]
):
    # type: (...) -> typing.Iterable[AddrRangeT]
    desired_range = get_desired_range(addrs)
    for node in nodes:
        node_addr = node.address
        if node_addr is not None:
            node_size = node.size
            assert node_size is not None
            node_range = range(node_addr, node_addr + node_size)
            if range(
                max(desired_range.start, node_range.start),
                min(desired_range.stop, node_range.stop),
            ):
                yield node


def nodes_at(
    nodes,  # type: typing.Iterable[AddrRangeT]
    addrs,  # type: typing.Union[int, range]
):
    # type: (...) -> typing.Iterable[AddrRangeT]
    desired_range = get_desired_range(addrs)
    for node in nodes:
        node_addr = node.address
        if node_addr is not None and node_addr in desired_range:
            yield node


def _address_interval(node):
    # type: (AddrRange) -> typing.Optional[intervaltree.Interval]
    """
    Creates an interval tree interval based on a GTIRB node's address and
    size or returns None, if the node has no address.
    """
    node_address = node.address
    if node_address is not None:
        node_size = node.size
        assert node_size is not None
        return intervaltree.Interval(
            node_address, node_address + node_size + 1, node
        )
    else:
        return None


class OffsetRange(typing_extensions.Protocol):
    """An object spanning a range of offsets."""

    @property
    def offset(self):
        # type: () -> int
        ...

    @property
    def size(self):
        # type: () -> int
        ...


def _offset_interval(node):
    # type: (OffsetRange) -> intervaltree.Interval
    """
    Creates an interval tree interval based on a GTIRB node's offset and size.
    """
    return intervaltree.Interval(
        node.offset, node.offset + node.size + 1, node
    )


def _nodes_on_interval_tree(
    tree,  # type: intervaltree.IntervalTree
    addrs,  # type: typing.Union[int, range]
    adjustment=0,  # type: int
):
    # type: (...) -> typing.Iterable
    """
    Implements nodes_on for an IntervalTree.
    :param tree: The IntervalTree to search.
    :param addrs: The address or addresses to locate nodes on.
    :param adjustment: An adjustment to be applied to the search range before
           consulting the interval tree.
    """

    desired_range = get_desired_range(addrs)
    for interval in tree.overlap(
        desired_range.start + adjustment, desired_range.stop + adjustment
    ):
        node = interval.data
        if node.address is None:
            continue

        # We explicitly exclude zero-sized blocks to match the existing
        # nodes_on function and prior behavior of callers before they switched
        # to using an interval tree.
        if not node.size:
            continue

        # Our interval tree ranges are closed, so we need to make sure not to
        # return items the caller didn't request.
        if node.address + node.size <= desired_range.start:
            continue

        yield node


def _nodes_at_interval_tree(
    tree,  # type: intervaltree.IntervalTree
    addrs,  # type: typing.Union[int, range]
    adjustment=0,  # type: int
):
    # type: (...) -> typing.Iterable
    """
    Implements nodes_at for an IntervalTree.
    :param tree: The IntervalTree to search.
    :param addrs: The address or addresses to locate nodes at.
    :param adjustment: An adjustment to be applied to the search range before
           consulting the interval tree.
    """

    desired_range = get_desired_range(addrs)
    for interval in tree.overlap(
        desired_range.start + adjustment, desired_range.stop + adjustment
    ):
        # Check that it's actually in our desired range, which may have a
        # step value that excludes it. This is a constant time operation.
        if interval.data.address in desired_range:
            yield interval.data


def symbolic_expressions_at(
    nodes,  # type: typing.Iterable[_SymbolicExpressionContainer[T_cov]]
    addrs,  # type: typing.Union[int, range]
):
    # type: (...) -> typing.Iterable[T_cov]
    return itertools.chain.from_iterable(
        node.symbolic_expressions_at(addrs) for node in nodes
    )
