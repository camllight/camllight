.TH CAMLC 1

.SH NAME
camlc \- The Caml Light compiler


.SH SYNOPSIS
.B camlc
[
.B \-cgipv
]
[
.B \-custom
]
[
.BI \-o \ exec-file
]
[
.BI \-I \ lib-dir
]
[
.BI \-O \ module-set
]
.I filename ...


.SH DESCRIPTION

.B camlc
is the standalone compiler for Caml Light, an implementation of the ML
language. (For interactive use of Caml Light, see 
.BR camllight (1).)
.B camlc
has a command-line interface similar to the C compiler 
.BR cc (1).
It accepts several types of arguments: source programs, module
interfaces, compiled object programs.

Arguments ending in .mli
are taken to be the sources of module
interfaces, declaring public global identifiers, concrete types,...
From the file 
.IR xxx \&.mli,
camlc produces a compiled interface, in the file
.IR xxx \&.zi.

Arguments ending in .ml
are taken to be the sources of module
implementations, defining global identifiers, concrete types, and
giving expressions to be evaluated. From the file
.IR xxx \&.ml,
camlc produces a compiled object code, in the file
.IR xxx \&.zo,
and a compiled interface
.IR xxx \&.zi.
The compiled interface is not produced if
the interface of the module is explicitly given, that is, if file
.IR xxx \&.mli
exists; then, the module implementation
.IR xxx \&.ml
is checked against the compiled interface
.IR xxx \&.zi,
which is assumed to exist.
However, if no source interface exists, then camlc produces a compiled
interface
.IR xxx \&.zi,
that exports everything that is defined in the implementation
.IR xxx \&.ml.

Arguments ending in .zo
are taken to be compiled object code. These
files are linked together, along with the object code files obtained
by compiling .ml
arguments (if any), and parts of the Caml Light library,
to produce a standalone executable program. The order in which .zo
and .ml
arguments are presented on the command line is relevant:
global identifiers are initialized in that order at run-time,
and it is a link-time error to use a global identifier before having
initialized it.

Arguments ending in .o or .a
are assumed to be native object code files, and are passed to the 
.BR cc (1)
compiler when linking a custom run-time system (option
.BR \-custom ).
Arguments ending in .c
are assumed to be C code source files, and
are passed to the
.BR cc (1)
compiler for compilation; the resulting .o file is then linked as
described previously.

The output of the linking phase is a file containing bytecode
executable by the Caml Light bytecode interpreter 
.BR camlrun (1). 
On most systems,
the bytecode file can be run directly: it has the executable bit set,
and it manages to launch the bytecode interpreter by itself. If this
fails, the bytecode file must be given as first argument to the
bytecode interpreter 
.BR camlrun (1).

.SH OPTIONS

.TP
.B \-c
Compile only. Suppresses the linking phase of the compilation.
.TP
.B \-custom
Link in custom runtime mode. In this mode, the linker produces an
output file that contains both the bytecode for the program and a
runtime system that contains exactly those C primitives that are
required by the program. This option is required when linking Caml
Light code with user-defined C functions.
.TP
.B \-g
Produce extra debugging information during compilation and linking.
.TP
.B \-i
Cause the compiler to print on standard output the types, exceptions
and global variables defined, with their inferred type, when compiling
an implementation (.ml file).
.TP
.BI \-I \ lib-dir
Adds
.I lib-dir 
to the path of directories searched for compiled interface files .zi
and compiled object files .zo. The default is to search in the current
directory first, then in the Caml Light library.
.TP
.BI \-o \ exec-file
Specifies the name of the output executable file. The default is
a.out.
.TP
.BI \-O \ module-set
Specifies which set of standard library modules are to be
implicitly "opened" at the beginning of a compilation, and linked with
the user's object code. There are three module sets
currently available: "cautious" and "fast", and "none".
"Cautious" and "fast" both provide the same standard operations on
integers, floating-point numbers, characters and character
strings, polymorphic lists and vectors, ..., as well as basic
input/output, exception handling, ... Modules from the "cautious"
set perform range and bound checking on string and vector
operations, and various sanity checks on their arguments as well,
while those from the "fast" set don't check anything. The default
is "cautious". The "-O none" option suppresses all default module
opening; compilation starts in an (almost) empty environment, and
no library code is linked. It is not of general use, except to
compile the standard library itself.
.TP
.B \-p
Compile and link in profiling mode.
.TP
.B \-v
Prints the compiler version number.

.SH FILES

.ta 3.5i
/usr/local/lib/caml-light	standard library
.br
/usr/local/bin/camlrun	the bytecode interpreter
.br
/usr/local/bin/camlc	the compiler driver
.br
/usr/local/lib/caml-light/camlcomp	the compiler itself
.br
/usr/local/lib/caml-light/camllink	the bytecode linker
.br

.SH SEE ALSO

.B camllight
(1),
.B camlrun
(1).
.br
.I
The Caml Light user's manual,
.P
chapter "Batch compilation".

.SH BUGS

Structured input/output is not type-safe; nothing prevents the user
from writing data of one type and reading it with another type.

Various size limitations are not checked by the compiler: the size of
a local environment should not exceed 256; the number of constructors
in a concrete type should not exceed 253; the number of global values
should be less than 65536; etc.

The module system is not foolproof, since it relies on the (base) names
of the files. Anything can happen if the user monkeys with the interface
path, or renames compiled interfaces. Also, the linking phase does not
check that compiled implementations are up to date w.r.t. compiled
interfaces.
