.TH CAMLLIBR 1

.SH NAME
camllibr \- Create libraries of Caml Light modules

.SH SYNOPSIS
.B camllibr
[
.BI \-I \ lib-dir
]
[
.BI \-o \ library-file
]
.IR file1 \&.zo
.IR file2 \&.zo
...

.SH DESCRIPTION

.B camllibr
packs in one single file a set of bytecode object files (.zo files).
The resulting file is also a bytecode object file, and also has
the .zo extension. It can be passed to the link phase of the
.BR camlc (1)
compiler in replacement of the original set of bytecode object files.

The order in which .zo arguments are presented on the command line is
relevant: global identifiers are initialized in that order at
run-time, and it is a link-time error to use a global identifier
before having initialized it.

.SH OPTIONS

.TP
.BI \-I \ lib-dir
Adds
.I lib-dir 
to the path of directories searched for compiled object files .zo. The
default is to search in the current directory first, then in the Caml
Light library (usually /usr/local/lib/caml-light). A directory added
by -I is searched after the current directory, but before the library.
.TP
.BI \-o \ exec-file
Specifies the name of the output file containing the library. The
default is library.zo.

.SH SEE ALSO

.B camlc
(1).
.br
.I
The Caml Light user's manual,
.P
chapter "The librarian".

