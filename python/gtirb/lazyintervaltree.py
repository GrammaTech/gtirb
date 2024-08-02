"""
Implements a simple wrapper that lazily initializes and updates an
IntervalTree.

GTIRB uses IntervalTrees to accelerate certain operations. However, these
operations are not always needed for a given GTIRB object or by a given GTIRB
analysis. To prevent scripts that do not need the IntervalTrees from wasting
time updating the data structures, the LazyIntervalTree in this module delays
instantiating or updating the tree. Instead, it queues the updates so they can
be rapidly applied when the script invokes an operation that requires an
up-to-date tree.
"""

import enum
from typing import (
    Collection,
    Generic,
    Iterator,
    List,
    Optional,
    Tuple,
    TypeVar,
)

from intervaltree import Interval, IntervalTree
from typing_extensions import Protocol

_K = TypeVar("_K")
_Kco = TypeVar("_Kco", covariant=True)
_V = TypeVar("_V")


class _EventType(enum.Enum):
    """Whether an interval is to be added or discarded."""

    ADDED = enum.auto()
    DISCARDED = enum.auto()


class IntervalBuilder(Protocol[_Kco, _V]):
    """Gets an interval for certain values.

    If no interval is available for a particular value, returns None instead.
    """

    def __call__(self, node: _V) -> Optional["Interval[_Kco, _V]"]:
        ...


class LazyIntervalTree(Generic[_K, _V]):
    """Simple wrapper to lazily initialize and update an IntervalTree.

    The underlying IntervalTree can be retrieved by calling get(). This will
    ensure that the tree is up-to-date with all intermediate modifications
    before returning it.

    In many algorithms, the tree may receive large numbers of modifications,
    adding and removing the same intervals several times before querying. In
    these cases, it may be faster to rebuild the tree from scratch rather than
    perform all of the intermediate modifications. For this reason, get() is
    not guaranteed to always return the same tree object. That is, the tree
    returned by get() should not be cached; calling get() may return a new tree
    rather than updating the tree it returned previously.
    """

    def __init__(
        self,
        values: Collection[_V],
        make_interval: IntervalBuilder[_K, _V],
    ):
        """Create a new lazy tree.

        :param values: collection of values from which the tree can be rebuilt
        :param make_interval: callable to get an interval for a value
        """
        self._interval_index: Optional["IntervalTree[_K, _V]"] = None
        self._interval_events: List[Tuple[_EventType, "Interval[_K, _V]"]] = []
        self._value_collection = values
        self._make_interval = make_interval

    def add(self, value: _V) -> None:
        """Add a value to the tree."""
        interval = self._make_interval(value)
        if interval is not None:
            self._interval_events.append((_EventType.ADDED, interval))

    def discard(self, value: _V) -> None:
        """Remove a value from the tree.

        Does nothing if the interval with that value is not present.
        """
        interval = self._make_interval(value)
        if interval is not None:
            self._interval_events.append((_EventType.DISCARDED, interval))

    def get(self) -> "IntervalTree[_K, _V]":
        """Get the most up-to-date tree reflecting all pending updates."""

        def intervals() -> Iterator["Interval[_K, _V]"]:
            for value in self._value_collection:
                interval = self._make_interval(value)
                if interval:
                    yield interval

        if self._interval_index is None:
            self._interval_index = IntervalTree(intervals())
        elif len(self._value_collection) <= len(self._interval_events):
            # Constructing a new tree involves one update for each value.
            self._interval_index = IntervalTree(intervals())
        else:
            # There are fewer updates than constructing a new tree would use.
            for event, interval in self._interval_events:
                if event == _EventType.ADDED:
                    self._interval_index.add(interval)
                else:
                    self._interval_index.discard(interval)
        self._interval_events.clear()
        return self._interval_index
