from typing import Generic, TypeVar

PointT = TypeVar("PointT", covariant=True)
DataT = TypeVar("DataT", covariant=True)

class Interval(Generic[PointT, DataT]):
    begin: PointT
    end: PointT
    data: DataT
    def __init__(self, begin: PointT, end: PointT, data: DataT): ...
    def length(self) -> PointT: ...
