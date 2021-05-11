import enum
import functools

import gtirb


class SearchScope(enum.Enum):
    ir = 1
    module = 2
    section = 3
    byte_interval = 4

    def select(self, ir, m, s, bi):
        """
        Given an IR, Module, Section, and ByteInterval, returns the object
        that matches this scope.
        """
        if self == SearchScope.ir:
            return ir
        if self == SearchScope.module:
            return m
        if self == SearchScope.section:
            return s
        if self == SearchScope.byte_interval:
            return bi
        assert False


def parameterize_one(name, values):
    """
    A decorator that paramaterizes a test case.
    :param name: The parameter name to parameterize.
    :param values: The values to supply to the parameter.
    """

    def decorator(f):
        @functools.wraps(f)
        def run_test(self, *args, **kwargs):
            for value in values:
                arg = {name: value}
                with self.subTest(**arg):
                    f(self, *args, **arg, **kwargs)

        return run_test

    return decorator


def create_interval_etc(address, size):
    """
    Creates a byte interval and all of the containing structures.
    """
    ir = gtirb.IR()
    m = gtirb.Module(name="test", ir=ir)
    s = gtirb.Section(name=".text", module=m)
    bi = gtirb.ByteInterval(address=address, size=size, section=s)
    return ir, m, s, bi
