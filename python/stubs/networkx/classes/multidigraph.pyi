from typing import (
    Collection,
    Dict,
    Generic,
    Iterable,
    Tuple,
    TypeVar,
    overload,
)
from typing_extensions import Literal

NodeT = TypeVar("NodeT")
KeyT = TypeVar("KeyT")
DataT = TypeVar("DataT")

class MultiDiGraph(Generic[NodeT, KeyT, DataT]):
    def __contains__(self, n: object) -> bool: ...
    def __getitem__(
        self, n: NodeT
    ) -> Dict[NodeT, Dict[KeyT, Dict[str, DataT]]]: ...
    def add_edge(
        self, u: NodeT, v: NodeT, key: KeyT | None = ..., **attr: DataT
    ) -> KeyT: ...
    def remove_edge(
        self, u: NodeT, v: NodeT, key: KeyT | None = ...
    ) -> None: ...
    def number_of_edges(
        self, u: NodeT | None = ..., v: NodeT | None = ...
    ) -> int: ...
    def clear(self) -> None: ...
    # The actual types of edges(), out_edges(), and in_edges() are remarkably
    # complicated. Depending on various combinations of arguments, they can
    # retrieve a collection of tuples of two, three, or four elements. These
    # overloads are specifically just the cases currently used by CFG, and do
    # not remotely cover all valid combinations of arguments.
    @overload
    def edges(
        self,
        nbunch: Iterable[NodeT] | NodeT | None = ...,
        data: Literal[False] = ...,
    ) -> Collection[Tuple[NodeT, NodeT]]: ...
    @overload
    def edges(
        self,
        nbunch: Iterable[NodeT] | NodeT | None = ...,
        *,
        data: str,
        default: DataT | None = ...,
    ) -> Collection[Tuple[NodeT, NodeT, DataT | None]]: ...
    def out_edges(
        self,
        nbunch: Iterable[NodeT] | NodeT | None = ...,
        *,
        data: str = ...,
        default: DataT | None = ...,
    ) -> Collection[Tuple[NodeT, NodeT, DataT | None]]: ...
    def in_edges(
        self,
        nbunch: Iterable[NodeT] | NodeT | None = ...,
        *,
        data: str = ...,
        default: DataT | None = ...,
    ) -> Collection[Tuple[NodeT, NodeT, DataT | None]]: ...
