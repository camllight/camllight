### Configuration section

# Which packages to compile and install.
# See the file README for a description of the packages and their requirements.
# Remember that "libunix" is required for
# "debugger", "libgraph" and "search_isos".
PACKAGES=libunix libgraph debugger libnum libstr mletags search_isos \
  caml-tex camlmode lorder profiler

# Some "make"s need this to ensure that they call /bin/sh, not /bin/csh
# Seems harmless on most other "make"s. Try removing this line if you
# run into trouble.
SHELL=/bin/sh

# Other configuration info is in Makefile.conf.
# Please edit Makefile.conf as well.

### End of configuration section

SUBDIRS=libunix libgraph debugger libnum libstr mletags search_isos \
  caml-tex camlmode lorder profiler

MKF=-f ../Makefile.conf -f Makefile

# Compile everything
all:
	@set -e; set -x; for d in $(PACKAGES); do (cd $$d; $(MAKE) $(MKF) all); done

# Install everything
install:
	@set +e; set -x; for d in $(PACKAGES); do (cd $$d; $(MAKE) $(MKF) install); done

# Remove all generated files
clean:
	@set +e; set -x; for d in $(SUBDIRS); do (cd $$d; $(MAKE) $(MKF) clean); done

# Rebuild the dependencies
depend:
	@set +e; set -x; for d in $(SUBDIRS); do (cd $$d; $(MAKE) $(MKF) depend); done
