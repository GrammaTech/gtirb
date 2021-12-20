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
S = typing.TypeVar("S")


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

    def symbolic_expressions_at(
        self, addrs: typing.Union[int, range]
    ) -> typing.Iterable[T_cov]:
        ...  # pragma: no cover


class ListWrapper(typing.MutableSequence[T]):
    def __init__(self, *args: typing.Iterable[T]):
        self._data: typing.List[T] = []
        for values in args:
            for value in values:
                self.append(value)

    def _add(self, value: T) -> None:
        pass  # pragma: no cover

    def _remove(self, value: T) -> None:
        pass  # pragma: no cover

    # begin functions for ABC
    @typing.overload
    def __getitem__(self, i: int) -> T:
        ...  # pragma: no cover

    @typing.overload
    def __getitem__(self, i: slice) -> typing.MutableSequence[T]:
        ...  # pragma: no cover

    def __getitem__(
        self, i: typing.Union[int, slice]
    ) -> typing.Union[T, typing.MutableSequence[T]]:
        return self._data[i]

    @typing.overload
    def __setitem__(self, i: typing_extensions.SupportsIndex, v: T) -> None:
        ...  # pragma: no cover

    @typing.overload
    def __setitem__(self, i: slice, v: typing.Iterable[T]) -> None:
        ...  # pragma: no cover

    def __setitem__(
        self,
        i: typing.Union[typing_extensions.SupportsIndex, slice],
        v: typing.Union[T, typing.Iterable[T]],
    ) -> None:
        if isinstance(i, slice):
            assert isinstance(v, typing.Iterable)
            indices = range(*i.indices(len(self)))
            values = list(v)
        elif -len(self._data) <= i.__index__() < len(self._data):
            indices = range(i.__index__(), i.__index__() + 1)
            values = [typing.cast(T, v)]
        else:
            raise IndexError("list assignment index out of range")
        for index in indices:
            self._remove(self._data[index])
        for value in values:
            self._add(value)
        if isinstance(i, slice):
            self._data[i] = values
        else:
            self._data[i] = values[0]

    @typing.overload
    def __delitem__(self, i: int) -> None:
        ...  # pragma: no cover

    @typing.overload
    def __delitem__(self, i: slice) -> None:
        ...  # pragma: no cover

    def __delitem__(self, i: typing.Union[int, slice]) -> None:
        if isinstance(i, slice):
            indices = range(*i.indices(len(self)))
        else:
            indices = range(i, i + 1)
        for index in indices:
            self._remove(self._data[index])

        del self._data[i]

    def __len__(self) -> int:
        return len(self._data)

    def insert(self, i: int, v: T) -> None:
        self._add(v)
        return self._data.insert(i, v)

    # The version of typing.py which comes with python 3.5.2 doesn't provide
    # definitions for append or remove on MutableList, so we have to do it
    # ourselves.
    def append(self, v: T) -> None:
        self.insert(len(self), v)

    def remove(self, v: T) -> None:
        del self[self._data.index(v)]

    # extend is not in every version of Python 3, so list wrapper adds it here
    # itself.
    def extend(self, other: typing.Iterable[T]) -> None:
        for v in other:
            self.append(v)

    # end functions for ABC
    def __str__(self) -> str:
        return str(self._data)

    def __repr__(self) -> str:
        return repr(self._data)


# A type variable for the SetWrapper's "self" type. Since type variables cannot
# have a generic bound (see https://github.com/python/mypy/issues/2756), we
# need to use Any instead. Which we then have to tell mypy to ignore.
#
# Used in __ior__.
_SetWrapperSelf = typing.TypeVar(  # type: ignore[misc]
    "_SetWrapperSelf", bound="SetWrapper[typing.Any]"
)


class SetWrapper(typing.MutableSet[T]):
    def __init__(self, *args: typing.Iterable[T]):
        self._data: typing.Set[T] = set()
        for arg in args:
            for v in arg:
                self.add(v)

    # begin functions for ABC
    def __contains__(self, v: object) -> bool:
        return v in self._data

    def __iter__(self) -> typing.Iterator[T]:
        return iter(self._data)

    def __len__(self) -> int:
        return len(self._data)

    def add(self, v: T) -> None:
        return self._data.add(v)

    def discard(self, v: T) -> None:
        return self._data.discard(v)

    # end functions for ABC

    # The version of typing.py which comes with python 3.5.2 doesn't provide
    # definitions for __or__ or clear on MutableSet, so we have to do it
    # ourselves.
    def __or__(
        self, other: typing.AbstractSet[S]
    ) -> typing.Set[typing.Union[T, S]]:
        return self._data | other

    # The type declaration for __ior__ in typeshed's MutableSet is problematic.
    # MutableSet requires __ior__ to accept an AbstractSet[S], and return a
    # Mutable[T|S]. This results in a type error when the result is assigned
    # back to the original variable unless S is T. Since this can't actually be
    # used if S is not T and is much easier to implement if S is T anyway, we
    # declare it to only accept AbstractSet[T]. This causes mypy to report an
    # error for incompatible override types and different return types for
    # __ior__ and __or__, which we ignore.
    def __ior__(  # type: ignore
        self: _SetWrapperSelf, other: typing.AbstractSet[T]
    ) -> _SetWrapperSelf:
        for value in other:
            self.add(value)
        return self

    def pop(self) -> T:
        it = iter(self)
        # pop is documented as raising a KeyError if it's empty, not
        # StopIteration
        try:
            result = next(it)
        except StopIteration:
            raise KeyError
        self.discard(result)
        return result

    def clear(self) -> None:
        while self:
            self.pop()

    # For whatever reason, update isn't included as part of abc.MutableSet.
    def update(self, *others: typing.Iterable[T]) -> None:
        for other in others:
            for v in other:
                self.add(v)

    def __str__(self) -> str:
        return str(self._data)

    def __repr__(self) -> str:
        return repr(self._data)


class DictWrapper(typing.MutableMapping[K, V]):
    def __init__(self, *args: DictLike[K, V]):
        self._data: typing.MutableMapping[K, V] = {}
        # Create a temporary dictionary so we can uniformly access the items
        # and add them to _data.
        temp: typing.Dict[K, V] = dict(*args)
        for i, v in temp.items():
            self[i] = v

    # begin functions for ABC
    def __getitem__(self, i: K) -> V:
        return self._data[i]

    def __setitem__(self, i: K, v: V) -> None:
        self._data[i] = v

    def __delitem__(self, i: K) -> None:
        del self._data[i]

    def __iter__(self) -> typing.Iterator[K]:
        return iter(self._data)

    def __len__(self) -> int:
        return len(self._data)

    # end functions for ABC
    def __str__(self) -> str:
        return str(self._data)

    def __repr__(self) -> str:
        return repr(self._data)


InstanceT = typing.TypeVar("InstanceT")
AttributeT = typing.TypeVar("AttributeT")


class IndexedContainer(typing_extensions.Protocol[T_contra]):
    """Container wth an index that can be updated."""

    def _index_discard(self, instance: T_contra) -> None:
        ...  # pragma: no cover

    def _index_add(self, instance: T_contra) -> None:
        ...  # pragma: no cover


class ParentGetter(typing_extensions.Protocol[T_contra]):
    """Interface for getting an _IndexedContainer for an instance."""

    def __call__(
        self, instance: T_contra
    ) -> typing.Optional[IndexedContainer[T_contra]]:
        ...  # pragma: no cover


class _IndexedAttribute(typing.Generic[AttributeT]):
    """
    The _IndexedAttribute descriptor notifies a parent when the attribute is
    modified. The outer class is generic in the attribute type and provides a
    __call__ method to deduce the remaining type parameters and construct the
    descriptor itself.

    Example usage:

    class Foo:
        my_int = _IndexedAttribute[int]()(lambda foo: foo.parent)
    """

    class Descriptor(typing.Generic[InstanceT]):
        """
        A descriptor that will notify a parent when the value is set and can be
        otherwise used like a normal attribute.
        """

        def __init__(self, parent_getter: ParentGetter[InstanceT]):
            self.parent_getter = parent_getter

        def __get__(
            self, instance: InstanceT, owner: typing.Type[InstanceT] = None,
        ) -> AttributeT:
            return getattr(instance, self.attribute_name)

        def __set__(self, instance: InstanceT, value: AttributeT) -> None:
            parent = self.parent_getter(instance)
            if parent:
                parent._index_discard(instance)
            setattr(instance, self.attribute_name, value)
            parent = self.parent_getter(instance)
            if parent:
                parent._index_add(instance)

        def __delete__(self, instance: InstanceT) -> None:
            raise AttributeError("can't delete attribute %s" % (self.name))

        def __set_name__(self, owner: InstanceT, name: str) -> None:
            self.name = name
            self.attribute_name = "_" + name

    def __call__(self, parent_getter: ParentGetter[InstanceT]) -> AttributeT:
        """
        Create the descriptor, but tell mypy it is the attribute type.

        The cast helps mypy recognize when the instance type satisfies a
        protocol. Mypy checks to see if the class matches the protocol instead
        of the instance. However, descriptors are treated as the descriptor
        type in the class and the attribute type in the instance. This causes
        mypy to reject the protocol, even though it works correctly at runtime.
        """
        return typing.cast(
            AttributeT, self.Descriptor[InstanceT](parent_getter)
        )


def get_desired_range(addrs: typing.Union[int, range]) -> range:
    if isinstance(addrs, int):
        return range(addrs, addrs + 1)
    else:
        return addrs


class AddrRange(typing_extensions.Protocol):
    """An object spanning a range of addresses."""

    # Protocol field types must match exactly, but properties are allowed to
    # return subtypes. This means that a class whose address or size is an int
    # will match Optional[int] properties, but not Optional[int] fields.

    @property
    def address(self) -> typing.Optional[int]:
        ...  # pragma: no cover

    @property
    def size(self) -> typing.Optional[int]:
        ...  # pragma: no cover


# Need a TypeVar bounded by the protocol so that nodes_on callers will get
# back the actual node type, not an AddrRange.
AddrRangeT = typing.TypeVar("AddrRangeT", bound=AddrRange)


def nodes_on(
    nodes: typing.Iterable[AddrRangeT], addrs: typing.Union[int, range],
) -> typing.Iterable[AddrRangeT]:
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
    nodes: typing.Iterable[AddrRangeT], addrs: typing.Union[int, range],
) -> typing.Iterable[AddrRangeT]:
    desired_range = get_desired_range(addrs)
    for node in nodes:
        node_addr = node.address
        if node_addr is not None and node_addr in desired_range:
            yield node


def _address_interval(
    node: AddrRangeT,
) -> "typing.Optional[intervaltree.Interval[int, AddrRangeT]]":
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
    def offset(self) -> int:
        ...  # pragma: no cover

    @property
    def size(self) -> int:
        ...  # pragma: no cover


OffsetRangeT = typing.TypeVar("OffsetRangeT", bound=OffsetRange)


def _offset_interval(
    node: OffsetRangeT,
) -> "intervaltree.Interval[int, OffsetRangeT]":
    """
    Creates an interval tree interval based on a GTIRB node's offset and size.
    """
    return intervaltree.Interval(
        node.offset, node.offset + node.size + 1, node
    )


def _nodes_on_interval_tree(
    tree: "intervaltree.IntervalTree[int, AddrRangeT]",
    addrs: typing.Union[int, range],
    adjustment: int = 0,
) -> typing.Iterable[AddrRangeT]:
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
    tree: "intervaltree.IntervalTree[int, AddrRangeT]",
    addrs: typing.Union[int, range],
    adjustment: int = 0,
) -> typing.Iterable[AddrRangeT]:
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
    nodes: typing.Iterable[_SymbolicExpressionContainer[T_cov]],
    addrs: typing.Union[int, range],
) -> typing.Iterable[T_cov]:
    return itertools.chain.from_iterable(
        node.symbolic_expressions_at(addrs) for node in nodes
    )
