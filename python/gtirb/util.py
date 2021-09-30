"""General utilities usable by any other GTIRB submoudle."""

import itertools
import typing

import intervaltree

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
ParentT = typing.TypeVar("ParentT")
AttributeT = typing.TypeVar("AttributeT")


class _IndexedAttribute(typing.Generic[AttributeT, InstanceT, ParentT]):
    """
    A descriptor that will notify a parent when the value is set and can be
    otherwise used like a normal attribute.
    """

    ParentGetterT = typing.Callable[[InstanceT], typing.Optional[ParentT]]

    def __init__(self, name, parent_getter):
        # type: (str, "ParentGetterT") -> None
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


def _address_interval(node):
    # type: ("Node") -> typing.Optional[intervaltree.Interval]
    """
    Creates an interval tree interval based on a GTIRB node's address and
    size or returns None, if the node has no address.
    """
    if node.address:
        return intervaltree.Interval(
            node.address, node.address + node.size + 1, node
        )
    else:
        return None


def _offset_interval(node):
    # type: ("Node") -> intervaltree.Interval
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
        if not node.address:
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
    nodes,  # type: typing.Iterable
    addrs,  # type: typing.Union[int, range]
):
    # type: (...) -> typing.Iterable[typing.Tuple]
    return itertools.chain.from_iterable(
        node.symbolic_expressions_at(addrs) for node in nodes
    )
