.TH CAMLRUN 1

.SH NAME
camlrun \- The Caml Light runtime system and bytecode interpreter

.SH SYNOPSIS
.B camlrun
[
.B \-v
]
.I bytecode-file
.I arguments ...

.SH DESCRIPTION

.B camlrun
executes a bytecode file, as produced by the linking phase of the
.BR camlc (1)
compiler. Most of the time, bytecode files are scripts that
are executable, and manage to launch
.B camlrun
by themselves. In some cases, the user may have to invoke
.B camlrun
by hand, to gain access to the bytecode interpreter options, or when
bytecode files fail to execute due to a non-standard installation.

The first non-option argument on the command line is taken to be the
name of the bytecode file to run. The remaining arguments on the
command line are directly passed to the Caml Light program.

.SH OPTIONS
.TP
.B \-v
Verbose mode. When set, the memory allocator and garbage collector
prints various diagnostic messages on error output.

.SH SEE ALSO

.BR camlc (1).
.br
.I
The Caml Light user's manual,
.P
chapter "Runtime system".
