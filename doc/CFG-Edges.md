Advice on when to Place ICFG Edges            {#CFG-Edges}
==================================

It is not always straightforward when to construct ICFG edges.  For
example, should there be an edge from a function's return to every
caller of that function?  While GTIRB does not preclude or enforce any
particular ICFG connection strategy, we do propose the following
suggestions for how to handle many common situations.

## Undefined/extern functions.

How to handle edges leaving call instructions to undefined or extern
functions?  In this case the function is not included in the module
and thus is not a possible edge target in the ICFG.

> ????

## Fall through edges from call sites.

Since we're working with an interprocedural CFG, we have edges from
call instructions to their targets. Do we also have edges from a call
instruction to the instruction immediately after it in memory?

> Yes unless we know the callee does not return (e.g., exit).

### Non-return functions.

When generating code around non-return functions (like exit):
sometimes compilers omit any following code if they know a call will
not return.  For example in a situation like this (except in machine
code):

```C
void foo()
{
    if (something) {
        bar_that_always_calls_exit();
        // stack cleanup
        return;
    }
}
```

In some cases, the compiler will know that
`bar_that_always_calls_exit()` will never return. In some cases, the
compiler doesn't figure that out. In the former case, the compiler may
omit the "stack cleanup" and "return" code. In the latter case, it
will not.

In some cases, your disassembly engine will know that
`bar_that_always_calls_exit()` will never return.  In some cases, your
disassembly engine doesn't figure that out.  In the former case, you
can choose to omit the "fall-through" edge in your IR. In the latter
case, you won't, because you think the execution flow is possible.

So, if you get the combination: the compiler figured it out and you
did *not*.  Then you will add the CFG edge from
`bar_that_always_calls_exit()` to whatever follows.  But whatever
follows will not necessarily be code in the function `foo()` or code
at all really.  It will be whatever random chunk of bytes the compiler
happened to plop down next.

We bring this up because it has bearing on your stance for adding
edges from callsites to their follows.  If you don't do that, you'll
never make this mistake.

However, we suggest that in the balance, having the edges when your
disassembly is correct outweighs the presence of this error.


## Edges from returns back to call sites.

What about from a return instruction to corresponding call sites
(plural) (or rather the instructions that follow them)?

> Yes.

## Edges from indirect calls to targets.

Indirect calls: we won't know who the targets are always. And we won't
know to add edges from corresponding return instructions.

> We put in edges (to and from) as we're able to identify them.  We
> don't conservatively put edges between every indirect call and every
> function.

## Tail calls.

Tail calls don't have return instructions or a place for a callee to
return to.

> Hook up the eventual return of a tail called function to the places
> it could lead (i.e., non tail-call callers and callers of tail-call
> callers).
