"""General utilities usable by any other GTIRB submoudle."""

import typing

K = typing.TypeVar("K")
V = typing.TypeVar("V")


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
