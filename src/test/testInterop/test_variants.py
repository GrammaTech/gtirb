import dataclasses
import os
import tempfile
from typing import Any, ClassVar

import gtirb


class V1(gtirb.Variant):
    type_list = [
        int,
        str,
    ]

    def __init__(self, val: Any):
        index = self.type_list.index(type(val))
        super().__init__(index, val)


def compare_variant_maps(m1, m2):
    assert not set(m1.keys()) ^ set(m2.keys())
    assert not set(v.val for v in m1.values()) ^ set(
        v.val for v in m2.values()
    )


def make_simple_variant_map():
    vmap = {1: V1(1), 2: V1("a"), 3: V1("z"), 4: V1(-1)}

    ad = gtirb.AuxData(vmap, "mapping<uint32_t,variant<int32_t,string>>")
    ir = gtirb.IR()
    ir.aux_data["simpleVariantMap"] = ad
    return ir, vmap


def test_mapvariant():
    ir, vmap = make_simple_variant_map()
    _, tmp = tempfile.mkstemp()
    try:
        ir.save_protobuf(tmp)
        ir2 = gtirb.IR.load_protobuf(tmp)
    finally:
        os.remove(tmp)
    ad2 = ir2.aux_data["simpleVariantMap"].data
    compare_variant_maps(ad2, vmap)


@dataclasses.dataclass
class I1:
    GTIRB_TYPE: ClassVar[str] = "int64_t"
    data: int


@dataclasses.dataclass
class I2:
    GTIRB_TYPE: ClassVar[str] = "int64_t"
    data: int


@dataclasses.dataclass
class S1:
    GTIRB_TYPE: ClassVar[str] = "string"
    data: str


@dataclasses.dataclass
class MyMap:
    TYPELIST = [
        I1,
        S1,
        I2,
    ]
    GTIRB_TYPE = "variant<" + ",".join(t.GTIRB_TYPE for t in TYPELIST) + ">"

    def __init__(self, map_) -> None:
        self._map = map_

    def __eq__(self, __o: object) -> bool:
        if not isinstance(__o, MyMap):
            return False
        return self._map == __o._map

    def __repr__(self) -> str:
        return f"MyMap({self._map!r})"

    def toAuxData(self):
        return {
            k: gtirb.Variant(self.TYPELIST.index(type(v)), v.data)
            for k, v in self._map.items()
        }

    @classmethod
    def fromAuxData(cls, auxdata: gtirb.AuxData):
        if not isinstance(auxdata.data, dict):
            raise Exception()
        newmap = dict()
        for (k, v) in auxdata.data.items():
            newmap[k] = cls.TYPELIST[v.index](v.val)
        return cls(newmap)


def make_complex_variant_map():
    variant_map = MyMap({"a": I1(1), "b": S1("hello"), "c": I2(4), "d": I1(0)})

    ad = gtirb.AuxData(
        data=variant_map.toAuxData(),
        type_name=f"mapping<string,{MyMap.GTIRB_TYPE}>",
    )
    ir = gtirb.IR()
    ir.aux_data["complexVariantMap"] = ad
    return ir, variant_map


def test_complex_variant_map():
    ir, variant_map = make_complex_variant_map()
    _, tmp = tempfile.mkstemp()
    try:
        ir.save_protobuf(tmp)
        ir2 = gtirb.IR.load_protobuf(tmp)
    finally:
        os.remove(tmp)
    ad2 = MyMap.fromAuxData(ir2.aux_data["complexVariantMap"])
    assert ad2 == variant_map


def write_ir(filename):
    ir, _ = make_simple_variant_map()
    ir2, _ = make_complex_variant_map()
    ir.aux_data["complexVariantMap"] = ir2.aux_data["complexVariantMap"]
    ir.save_protobuf(filename)


def check_ir(filename):
    ir = gtirb.IR.load_protobuf(filename)
    _, simple = make_simple_variant_map()
    compare_variant_maps(ir.aux_data["simpleVariantMap"].data, simple)
    my_map = MyMap.fromAuxData(ir.aux_data["complexVariantMap"])
    _, my_other_map = make_complex_variant_map()
    assert my_other_map == my_map, (my_other_map, my_map)


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("-r")
    parser.add_argument("-w")
    args = parser.parse_args()

    if args.r:
        check_ir(args.r)

    if args.w:
        write_ir(args.w)
