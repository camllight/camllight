### Configuration section

# Which packages to compile and install.
# See the file README for a description of the packages and their requirements.
# Remember that "libunix" is required for
# "debugger", "libgraph" and "search_isos".
PACKAGES=libunix libgraph debugger libnum libstr mletags search_isos \
  caml-tex camlmode

# Which C compiler to use.
# Should be the same compiler used to compile the main system (../src).
CC=cc

# Any additional options to $(CC).
OPTS=

# How to call the C preprocessor on a file that does not have the .c extension.
# That's /lib/cpp on most machines, sometimes /usr/bin/cpp,
# and /usr/ccs/lib/cpp under Solaris.
# The -P option suppresses the generation of "# linenum" directives,
# which are not understood by Caml Light.
CPP=/lib/cpp -P

# The directory where public executables will be installed
BINDIR=/usr/local/bin

# The directory where library files will be installed
LIBDIR=/usr/local/lib/caml-light

# The manual section where the manual pages will be installed
MANEXT=1

# The directory where the manual pages will be installed
MANDIR=/usr/local/man/man$(MANEXT)

# The path to the X11 include directory
# (usually /usr/include/X11; for SunOS, /usr/openwin/include/X11)
# Needed only for the "libgraph" package.
X11_INCLUDES=/usr/include/X11

# Name of the target architecture.
# Used only for the libnum library (arbitrary-precision arithmetic), to
# select assembly-language versions of time-critical low-level routines.
# Known targets: 68K, vax, ns, mips, alpha, pyramid, i960, sparc, supersparc.
# If you don't know, leave BIGNUM_ARCH=C, which selects a portable
# C implementation of these routines.
BIGNUM_ARCH=C

# Name of the directory where TeX input files should be installed.
# Needed only for the "caml-tex" package.
TEXINPUTDIR=/usr/local/tex/inputs

# Name of the directory where Emacs Lisp files should be installed.
# Needed only for the "camlmode" package.
EMACSLISPDIR=/usr/local/emacs/site-lisp

# Some "make"s need this to ensure that they call /bin/sh, not /bin/csh
# Seems harmless on most other "make"s. Try removing this line if you
# run into trouble.
SHELL=/bin/sh

### End of configuration section

SUBDIRS=libunix libgraph debugger libnum libstr mletags search_isos \
  caml-tex camlmode lorder profiler

DEFS=CC='$(CC)' OPTS='$(OPTS)' CPP='$(CPP)' BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) MANEXT=$(MANEXT) MANDIR=$(MANDIR) X11_INCLUDES=$(X11_INCLUDES) BIGNUM_ARCH=$(BIGNUM_ARCH) TEXINPUTDIR=$(TEXINPUTDIR) EMACSLISPDIR=$(EMACSLISPDIR)

# Compile everything
all:
	@set +e; set -x; for d in $(PACKAGES); do (cd $$d; $(MAKE) $(DEFS) all); done

# Install everything
install:
	@set +e; set -x; for d in $(PACKAGES); do (cd $$d; $(MAKE) $(DEFS) install); done

# Remove all generated files
clean:
	@set +e; set -x; for d in $(SUBDIRS); do (cd $$d; $(MAKE) clean); done

# Rebuild the dependencies
depend:
	@set +e; set -x; for d in $(SUBDIRS); do (cd $$d; $(MAKE) depend); done
