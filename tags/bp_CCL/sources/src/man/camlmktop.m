.TH CAMLMKTOP 1

.SH NAME
camlmktop \- Create custom Caml Light toplevel systems

.SH SYNOPSIS
.B camlmktop
[
.B \-cgiv
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

.B camlmktop
builds Caml Light toplevel systems that contain user code preloaded at
start-up. The filename arguments are either .zo files (compiled
bytecode files) or .o or .a files (compiled native code files). The
output of the
.B camlmktop
command is a toplevel system that can be launched by giving it as argument to
.BR camllight (1).

.SH OPTIONS

.TP
.B \-custom
Link in custom runtime mode. In this mode, the linker produces an
output file that contains both the bytecode for the program and a
runtime system that contains exactly those C primitives that are
required by the program. This option is required when linking Caml
Light code with user-defined C functions.
.TP
.BI \-I \ lib-dir
Adds
.I lib-dir 
to the path of directories searched for compiled interface files .zi
and compiled object files .zo. The default is to search in the current
directory first, then in the Caml Light library (usually
/usr/local/lib/caml-light). A directory added by -I is searched after
the current directory, but before the library. This option takes
effect for the compilation of subsequent .ml and .mli files, as well
as for the final linking.
.TP
.BI \-o \ exec-file
Specifies the name of the output file containing the toplevel system.
The default is camltop.out.

.SH SEE ALSO

.BR camllight (1),
.BR camlc (1).
.br
.I
The Caml Light user's manual,
.P
chapter "The toplevel system".

.SH BUGS

For each .zo file given as argument,
.B camlmktop
expects to find a .zi file with the same base name that contains the
compiled interface (export signature) corresponding to the .zo file.
Take this into account if you rename .zo files once compiled.
