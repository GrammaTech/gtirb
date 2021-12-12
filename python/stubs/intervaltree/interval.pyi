from typing import Generic, TypeVar

PointT = TypeVar("PointT")
DataT = TypeVar("DataT")

class Interval(Generic[PointT, DataT]):
    begin: PointT
    end: PointT
    data: DataT
    def __init__(self, begin: PointT, end: PointT, data: DataT): ...
