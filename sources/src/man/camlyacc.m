.TH CAMLYACC 1

.SH NAME
camlyacc \- a parser generator for Caml Light

.SH SYNOPSIS
.B camlyacc
[
.B \-vs
]
[
.BI \-b prefix
]
.I grammar.mly

.SH DESCRIPTION

.B camlyacc
produces a parser from a context-free grammar specification
with attached semantic actions, in the style of 
.BR yacc (1).
Assuming the
input file is
.IR grammar \&.mly
.B camlyacc
outputs the two files
.IR grammar \&.mli
and
.IR grammar \&.ml.
The two output files must be compiled by Caml Light, and
linked with the rest of the program.

The generated module defines one parsing function per entry point in the
grammar. These functions have the same names as the entry points.
Parsing functions take as arguments a lexical analyzer (a function
from lexer buffers to tokens) and a lexer buffer, and return the
semantic attribute of the corresponding entry point. Lexical analyzer
functions are usually generated from a lexer specification by
.BR camllex (1).
Lexer buffers are an abstract data type
implemented in the standard library "lexing". Tokens are values from
the concrete type "token", defined in the interface file
.IR grammar \&.mli 
produced by 
.B camlyacc.

.SH OPTIONS

.TP
.B -v
Generate a description of the parsing tables and a report on conflicts
resulting from ambiguities in the grammar. The description is put in
file grammar.output.

.TP
.B -s
Generate a 
.IR grammar \&.ml 
file with smaller phrases. Semantic actions are
presented in the 
.IR grammar \&.ml 
output file as one large vector of functions. By
default, this vector is built by a single phrase. When the grammar is
large, or contains complicated semantic actions, the resulting phrase
may require large amounts of memory to be compiled by Caml Light. With
the 
.B \-s 
option, the vector of actions is constructed incrementally,
one phrase per action. This lowers the memory requirements for the
compiler, but it is not possible anymore to infer the types of
nonterminal symbols: typechecking is turned off on symbols that do not
have a type specified by a %type directive.

.TP
.BI -b prefix
Name the output files
.IR prefix \&.ml,
.IR prefix \&.mli,
.IR prefix \&.output,
instead of the default naming convention.

.SH SEE ALSO
.BR camllex (1)
.br
.I
The Caml Light user's manual,
.P
chapter "Lexer and parser generators".
.br
.I
YACC - Yet Another Compiler Compiler,
.P
by S. C. Johnson.
.br
.I
Compilers - Principles, Techniques, and Tools
.P
(chapter 4), by A. V. Aho, R. Sethi, and J. D. Ullman.

.SH DIAGNOSTICS

The number of reduce-reduce conflicts, shift-reduce conflicts, and
unused rules is reported on the standard output. A more detailed
report can be obtained with the 
.B \-v
flag.

.SH BUGS

Error recovery is not implemented.

Type checking is performed when compiling the `.ml' output file, not
when compiling the grammar. This does not break type safety, since the
`.ml' output file is well-typed iff the input grammar is well-typed.
However, this results in type error messages that are somewhat hard to
relate to the input grammar.
