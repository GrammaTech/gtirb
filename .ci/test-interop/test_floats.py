import argparse

import gtirb


def create_floats(filename: str):
    ir = gtirb.IR()
    ir.aux_data["AFloat"] = gtirb.AuxData(0.5, "float")
    ir.save_protobuf(filename)  # fixme


def check_for_floats(filename: str) -> bool:
    ir = gtirb.IR.load_protobuf(filename)
    f = ir.aux_data["AFloat"]
    return f.type_name == "float" and f.data == 0.5


parser = argparse.ArgumentParser()
parser.add_argument("-w", required=False, type=str)
parser.add_argument("-r", required=False, type=str)

if __name__ == "__main__":
    args = parser.parse_args()
    if args.w:
        create_floats(args.w)
    elif args.r:
        if check_for_floats(args.r):
            exit(0)
        else:
            exit(1)
