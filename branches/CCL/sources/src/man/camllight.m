.TH CAMLLIGHT 1

.SH NAME
camllight \- The Caml Light toplevel system


.SH SYNOPSIS
.B camllight
[
.BI \-I \ lib-dir
]
[
.BI \-O \ module-set
]
[
.I special-toplevel
]

.SH DESCRIPTION

.B camllight
is the toplevel system for Caml Light, an implementation of the ML
language. The
.B camllight
command permits interactive use of Caml Light, through a Lisp-like
toplevel loop. It repeatedly reads phrases on standard input and
prints inferred types and result values on standard output.
End-of-file on standard input terminates 
.BR camllight .
(For batch-oriented use of Caml Light, see
.BR camlc (1).)

The optional
.I special-toplevel
argument is the name of a special toplevel system, as produced by
.BR camlmktop (1),
that should be executed. When omitted, the standard toplevel system is
executed.

.SH OPTIONS

The following options are interpreted by 
.BR camllight .

.TP
.B \-g
Start the toplevel system in debugging mode. This mode gives access to
values and types that are local to a module, that is, not exported by
the interface of the module. In particular, values of abstract types
are printed using their concrete representations, and the functions
local to a module can be traced.  This applies only to the modules
that have been compiled in debugging mode, that is, those modules that
have an associated .zix file.
.BI \-I \ lib-dir
Adds 
.I lib-dir
to the path of directories searched for compiled interface files .zi
and compiled object files .zo. The default is to search in the current
directory first, then in the Caml Light library (usually
/usr/local/lib/caml-light). A directory added by -I is searched after
the current directory, but before the library.
.TP
.BI -O \ module-set
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
no library code is linked. It is not of general use.

.SH FILES

.ta 3.5i
/usr/local/lib/caml-light	standard library
.br
/usr/local/bin/camlrun	the bytecode interpreter
.br
/usr/local/bin/camllight	the toplevel driver
.br
/usr/local/lib/caml-light/camltop	the standard toplevel
.br

.SH SEE ALSO

.BR camlc (1),
.BR camlrun (1),
.BR camlmktop (1).
.br
.I
The Caml Light user's manual,
.P
chapter "The toplevel system".

