GT-IRB
======

The GrammaTech Intermediate Representation for Binaries (GT-IRB) is a machine code analysis and rewriting infrastructure.  The GT-IRB is based on GrammaTech's CSurf/SWYX IR.

## Project Overview

We have promised to deliver a "binary code IR tailored for rewriting" called GT-IRB.  GT-IRB will be published and available for other performers to write front-ends, analyses, and back-ends against. GT-IRB will be based as much as possible on our existing CSurf/SWYX IR, specifically it will be the subset of our existing IR which we determine to be (i) general and (ii) minimally sufficient for binary rewriting.  Our efforts will include the following:

1.  Definition of a specification of GT-IRB.
2.  Refactoring of CSurf/SWYX to use the specified GT-IRB.
3.  Implementation of translation facilities to produce GT-IRB from the CSurf/SWYX front-end and consume GT-IRB in CSurf/SWYX back-end.
4.  Ongoing modularization of CSurf/SWYX into separately compilable components which communicate via GT-IRB.  An initial set of useful components may be: (i) a COTS-binary to GT-IRB front-end, (ii) a GT-IRB to GT-IRB backend, and (iii) a GT-IRB to assembler pretty printer.
