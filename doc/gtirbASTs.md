## Binary Nina

Binary ninja doesn't really have ASTs

Their "architecture plugins" more or less expose this interface

semantics :: Bytes -> EA -> (Length, IL)
text :: Bytes -> EA -> String

(where IL is their machine independent intermediate language)

It is possible that some implementations of this use ASTs internally
(and certainly wouldn't be surprising... there are a lot of APIs that all would
need to do decoding), but it isn't exported by the public API.

### RAMBLr, angr, capstone

Ramblr uses [angr](https://github.com/angr/angr) to do disassembly, angr uses (capstone)[http://www.capstone-engine.org/)

capstone decodes internally to this

```
// MCOperand - Instances of this class represent operands of the MCInst class.
/// This is a simple discriminated union.
struct MCOperand {
        enum {
                kInvalid = 0,                 ///< Uninitialized.
                kRegister,                ///< Register operand.
                kImmediate,               ///< Immediate operand.
                kFPImmediate,             ///< Floating-point immediate operand.
        } MachineOperandType;
        unsigned char Kind;

        union {
                unsigned RegVal;
                int64_t ImmVal;
                double FPImmVal;
        };
};

/// MCInst - Instances of this class represent a single low-level machine
/// instruction.
struct MCInst {
        unsigned OpcodePub;
        uint8_t size;   // number of operands
        bool has_imm;   // indicate this instruction has an X86_OP_IMM operand - used for ATT synta
        uint8_t op1_size; // size of 1st operand - for X86 Intel syntax
        unsigned Opcode;
        MCOperand Operands[48];
        cs_insn *flat_insn;     // insn to be exposed to public
        uint64_t address;       // address of this insn
        cs_struct *csh; // save the main csh
        uint8_t x86opsize;      // opsize for [mem] operand

        // (Optional) instruction prefix, which can be up to 4 bytes.
        // A prefix byte gets value 0 when irrelevant.
        // This is copied from cs_x86 struct
        uint8_t x86_prefix[4];
        uint8_t imm_size;       // immediate size for X86_OP_IMM operand
        bool writeback; // writeback for ARM
};
```

which eventually gets exposed via the public interface to this:

```
typedef struct cs_x86_op {
                x86_op_type type;       // operand type
                union {
                        x86_reg reg;    // register value for REG operand
                        int64_t imm;            // immediate value for IMM operand
                        double fp;              // floating point value for FP operand
                        x86_op_mem mem;         // base/index/scale/disp value for MEM operand
                };

                // size of this operand (in bytes).
                uint8_t size;

                // AVX broadcast type, or 0 if irrelevant
                x86_avx_bcast avx_bcast;

                // AVX zero opmask {z}
                bool avx_zero_opmask;
} cs_x86_op;

typedef struct cs_x86 {
        // Instruction prefix, which can be up to 4 bytes.
        // A prefix byte gets value 0 when irrelevant.
        // prefix[0] indicates REP/REPNE/LOCK prefix (See X86_PREFIX_REP/REPNE/LOCK above)
        // prefix[1] indicates segment override (irrelevant for x86_64):
        // See X86_PREFIX_CS/SS/DS/ES/FS/GS above.
        // prefix[2] indicates operand-size override (X86_PREFIX_OPSIZE)
        // prefix[3] indicates address-size override (X86_PREFIX_ADDRSIZE)
        uint8_t prefix[4];

        // Instruction opcode, which can be from 1 to 4 bytes in size.
        // This contains VEX opcode as well.
        // An trailing opcode byte gets value 0 when irrelevant.
        uint8_t opcode[4];

        // REX prefix: only a non-zero value is relevant for x86_64
        uint8_t rex;

        // Address size, which can be overridden with above prefix[5].
        uint8_t addr_size;

        // ModR/M byte
        uint8_t modrm;

        // SIB value, or 0 when irrelevant.
        uint8_t sib;

        // Displacement value, or 0 when irrelevant.
        int32_t disp;

        /* SIB state */
        // SIB index register, or X86_REG_INVALID when irrelevant.
        x86_reg sib_index;
        // SIB scale. only applicable if sib_index is relevant.
        int8_t sib_scale;
        // SIB base register, or X86_REG_INVALID when irrelevant.
        x86_reg sib_base;

        // SSE Code Condition
        x86_sse_cc sse_cc;

        // AVX Code Condition
        x86_avx_cc avx_cc;

        // AVX Suppress all Exception
        bool avx_sae;

        // AVX static rounding mode
        x86_avx_rm avx_rm;

        // Number of operands of this instruction,
        // or 0 when instruction has no operand.
        uint8_t op_count;

        cs_x86_op operands[8];  // operands for this instruction.
};
```

This is pretty similar to the way IDA does things (big struct, fixed size array of operands)

## IDA

The IDA struct is pretty big, but it is more or less

struct op_t {
    optype_t type; // reg, mem, imm, etc
    uval_t value;
    ... bunch of other stuff, some flags, etc ...
};
struct insn_t {
    ea_t ip;
    uint16_t opcode;
    uint16_t size;
    op_t Operands[6];
    ... bunch of other ida bookkeeping ....
};

## Nasm

Seems to be a recurring theme here (lots of these fields are just internal nasm state)

```
typedef struct operand { /* operand to an instruction */
    opflags_t       type;       /* type of operand */
    int             disp_size;  /* 0 means default; 16; 32; 64 */
    enum reg_enum   basereg;
    enum reg_enum   indexreg;   /* address registers */
    int             scale;      /* index scale */
    int             hintbase;
    enum eval_hint  hinttype;   /* hint as to real base register */
    int32_t         segment;    /* immediate segment, if needed */
    int64_t         offset;     /* any immediate number */
    int32_t         wrt;        /* segment base it's relative to */
    int             eaflags;    /* special EA flags */
    int             opflags;    /* see OPFLAG_* defines below */
    decoflags_t     decoflags;  /* decorator flags such as {...} */
} operand;

typedef struct insn { /* an instruction itself */
    char            *label;                 /* the label defined, or NULL */
    int             prefixes[MAXPREFIX];    /* instruction prefixes, if any */
    enum opcode     opcode;                 /* the opcode - not just the string */
    enum ccode      condition;              /* the condition code, if Jcc/SETcc */
    int             operands;               /* how many operands? 0-3 (more if db et al) */
    int             addr_size;              /* address size */
    operand         oprs[MAX_OPERANDS];     /* the operands, defined as above */
    extop           *eops;                  /* extended operands */
    int             eops_float;             /* true if DD and floating */
    int32_t         times;                  /* repeat count (TIMES prefix) */
    bool            forw_ref;               /* is there a forward reference? */
    bool            rex_done;               /* REX prefix emitted? */
    int             rex;                    /* Special REX Prefix */
    int             vexreg;                 /* Register encoded in VEX prefix */
    int             vex_cm;                 /* Class and M field for VEX prefix */
    int             vex_wlp;                /* W, P and L information for VEX prefix */
    uint8_t         evex_p[3];              /* EVEX.P0: [RXB,R',00,mm], P1: [W,vvvv,1,pp] */
                                            /* EVEX.P2: [z,L'L,b,V',aaa] */
    enum ttypes     evex_tuple;             /* Tuple type for compressed Disp8*N */
    int             evex_rm;                /* static rounding mode for AVX512 (EVEX) */
    int8_t          evex_brerop;            /* BR/ER/SAE operand position */
} insn;
```

## Proposal?

It seems something like struct { int opcode; operand [N]; } might be the
most convenient for people who are already using other tools. This should
also be somewhat compact to serialize (although we still might not want to
serialize them, instead decoding and applying symbolic hints when
requested).

I'm not that happy about potentially having yet another AST representation.
We could consider switching our existing ASTs to something like

operand : Operand(op_kind kind, INT64 n, ...)
instruction : Instruction(opcode op, operand op1, operand op2, operand op3)

(basically making the TSL asts directly mirror the "struct" representation)

This of course makes everything much less typesafe (although I actually
don't think the type safety buys us much), and means we can't quite as
easily do pattern matching on ASTs.

Could also relatively easily convert between the current TSL representation
and this. This would probably have about the same cost as just decoding
again (if we decided to store these rather than always redecoding from
bytes).

I think we could also relatively easily decode directly to this
representation (via an automatic conversion of the existing decoder to this
representation) if we wanted to provide a decoider to others.

I guess the big open question which remains is if we should store anything
in the IR.

I didn't get a chance to run any numbers on this but if the overhead of
serializing with this (struct) representation is low enough (wild guess
maybe 5x, assuming we do smart things like serialize with variable length
integers rather than the full 32 bits) it is probably worth storing ASTs of
this form in the IR. This makes it much easier to modify instructions and
add new ones (no need for an encoder, or to "allocate" memory to put them
in). Basically this struct would replace the fAST layer we have now (fAST
will need to stay around for the backend initially, but someday we could
write an LMI interface for the struct and eliminate fAST).

Now that I think about it, if we look at this as (eventually) replacing
fAST, at least they'll be no net increase in AST representations, which
alleviates one of my big concerns. 

I'd imagine we'd still mostly operate internally on TSL asts, and simply
view this an a serialization/external formal. Maybe we could gradually move
some things to operating on these directly though (but unless we want to
bite off converting the semantics to this, the existing TSL asts will have
to live on for some time).

(I'm sure this list of pros and cons is incomplete)

There were a lot of what ifs and maybes up there. I think (ha) what I am
tentatively proposing is storing "struct" ASTs in the IR and converting to
TSL ASTs when we work with them.

### Pros

* Simple external (API) representation, similar to other tools
* Compact (ish?) serialization
* Easy to add new instructions (well, easier than the decode on the fly way)

### Cons

* Unlike fAST (and formally, trestle), serialization format doesn't mirror
  the TSL asts
* Struct ASTs are much less typesafe (but maybe we don't care since we'll be
  converting to TSL)
* Yet another AST format
* Would need to write (and maintain) conversion function to/from TSL asts
  (with fAST, due to the grammar being idential to the thin ASTs we can
  generate these in ISAL). It might be possible to have a tool which
  automatically generates the conversion functions for the struct
  representation, but likely maintaining (and fixing bugs) in that tool
  would be just as much work as manually mantaining the conversion.


I'm not sure I've even sold myself on this proposal, but it might be the
best of many not quite perfect options.

## More concrete proposal

```
struct sym_info {
    Symbol *base;
    Symbol *minus; /* optional, if set, represents "base - minus"
    int64 offset;
};

struct operand {
    enum {
        NONE,
        REG_DIRECT,
        IMM,
        INDIRECT,
        // TODO: arm reg+shift
        SYMBOLIC = 0x80 /* or'd with above, or maybe just SYM_INDIRECT, etc */
    } kind;

    int8 size; /* 8, 16, 32, 64 */

    union {
        enum reg {
           REG_NONE,
           X86_RAX, X86_EAX, X86_AX, X86_AH,
           X86_MM0, ...
           X86_XMM0, ...
           ARM_R0, ...
        } reg_direct;

        union {
            uint64 imm;
            sym_info *sym_imm;
        };

        struct indirect {
            reg base;
            reg index; /* ... */
            int scale; /* or shift amount for arm */
            union {
                int64 offset;
                sym_info *sym_offset;
            };
            union {
                struct {
                    int x86_seg; /* CS, SS, DS, ... */
                };
                struct {
                    enum { NONE, LSL, LSR, ASR, ROR, RRX }  arm_shift_op;
                };
            };
        } indirect;
    }
    /* IDA also has:
        offb  // offset of operand relative to instruction start
       whchi might be useful
    */
};

struct instruction {
    eaT ea;
    int8 size;

    /* 
        X86_REP, X86_REPE, X86_REPNE, X86_LOCK
        ARM_{NE,EQ} // condition codes
    */
    int flags;

    int opcode; // union of {zero,one,two,three}OpInstr
    uint8 noperands;
    operand operands[4 /* four ought to be enough for anybody? */];

    /* Nasm also has:
        label (not needed, higher level)
        prefixes (folded into operand size, flags, etc */
        condition (for Jcc/SETcc, folded into opcode */
        addr_size (folded into operand)
        rep count (only for data)
        some rex/vex/evec stuff, folded into opcode/operand for us
    */

    /* IDA also has a couple "processor dependent" fields (with no defined
       use in the isa independent api), it isn't clear what can go here
       (IDA docs are horrible)
    */

    /* Capstone has:
        prefix bytes (see above)
        address size (see above)
        modrm/sib/disp (redundant with operands)
        condition code (see above)
    */        
};
```

## ISAL spec

Here is roughly how the isal tsl translations would map to the new structure
(if we wanted to decode directly to these asts)

Types of translation terms (in as TSL types => above):

* reg -> reg enum
* scale :: int8 -> unchanged
* addr -> struct indirect
* operand{8,16,32,64,...} -> struct operand
* {one,two,three}OpInstr -> int opcode
* offset :: int{8,16,32} -> unchanged
* instruction -> struct instruction

So the tsl types should directly line up with components of the above structure
I think the could mostly automatically "convert" the TSL translation expressions
to something to generate the above structure (some constructs would certainly need 
manual intervention though). If we want to allow decoding to both representations
we'd probably want to maintain both in the isal spec.

I don't think ISAL can help at all with the translation between tsl and 'struct'.
Well, *if* we had the capability of going "backwards" from the TSL ASTs back to isal
rules (the exact piece we'd need for an encoder incidentally) then it could, but this
piece probably isn't worth implementing just for the translation, the manual translation
function is easy enough to implement. 
