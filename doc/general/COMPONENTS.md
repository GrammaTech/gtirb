GTIRB Components
================

The pages in this section serve two purposes.

* A language-agnostic description of the representation components,
  with references to language-specific API implementations of these
  components.

* A normative description of required API features for GTIRB.
  If you implement a new API for GTIRB, it must satisfy all these requirements.



Components that must be represented
-----------------------------------

| Component                                 | Protobuf Message Type | Notes |
|-------------------------------------------|-----------------------|-------|
| [Node](Node.md)                           | -                     | Root class for many components |
| [IR](IR.md)                               | `IR`                  | A complete internal representation. |
| [Module](Module.md)                       | `Module`              | Represents a single binary (library or executable) |
| [Section](Section.md)                     | `Section`             | A named section of a binary. |
| [ByteInterval](ByteInterval.md)           | `ByteInterval`        | A contiguous region of bytes in a binary.|
| [CodeBlock](CodeBlock.md)                 | `CodeBlock`           | A basic block in the binary. |
| [DataBlock](DataBlock.md)                 | `DataBlock`           | A data object, possibly symbolic. |
| [ProxyBlock](ProxyBlock.md)               | `ProxyBlock`          | A placeholder to serve as the endpoint (source or target) of a [CfgEdge](CfgEdge.md). |
| [Symbol](Symbol.md)                       | `Symbol`              | Maps a name to an object in the IR. |
| [CFG](CFG.md)                             | `CFG`                 | The interprocedural control flow graph. |
| [AuxDataContainer](AuxDataContainer.md)   | -                     | Functionality for associating [auxiliary data](AuxData.md) with elements of the representation.      |
| [Block](Block.md)                         | -                     | Base class for blocks. |
| [ByteBlock](ByteBlock.md)                 | -                     | Base class for blocks that belong to a **ByteInterval** and store their bytes there. |
| [CfgNode](CfgNode.md)                     | -                     | A block that may appear as a vertex in the CFG. |
| [CfgEdge](CfgEdge.md)                     | `Edge`                | An edge in the CFG. |
| [CfgEdgeLabel](CfgEdgeLabel.md)           | `EdgeLabel`           | The label on a CfgEdge. |
| [SymbolicExpression](SymbolicExpression.md) | -                     | A data value or instruction operand which should be interpreted as referring to a symbol. |
| [SymStackConst](SymStackConst.md)         | `SymStackConst`       | A symbolic operand of the form "Sym + Offset", representing an offset from a stack variable. |
| [SymAddrConst](SymAddrConst.md)           | `SymAddrConst`        | A symbolic operand of the form "Sym + Offset". |
| [SymAddrAddr](SymAddrAddr.md)             | `SymAddrAddr`         | A symbolic operand of the form "(Sym1 - Sym2) / Scale + Offset". |
| [Version information](Version.md)         | -                     | The applicable GTIRB and Protobuf versions. |
