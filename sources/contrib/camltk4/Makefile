# WARNING: 
# These are for local compilation. They are overriden by Makefile
# in cl/contrib
CC=gcc
OPTS=
CPP=/lib/cpp -P
RANLIBTEST=test -f /usr/bin/ranlib -o -f /bin/ranlib
RANLIB=ranlib
MANEXT=1
MANDIR=/usr/local/man/man$(MANEXT)
BINDIR=/usr/local/bin
LIBDIR=/usr/local/lib/caml-light
X11_INCLUDES=/usr/include
X11_LIB=/usr/X11R6/lib
TCLINCLDIR=/usr/local/lib/tcl7.4/include
TKINCLDIR=/usr/local/lib/tk4.0/include
TCLLIBDIR=/usr/local/lib/tcl7.4/lib
TKLIBDIR=/usr/local/lib/tk4.0/lib
#Does not work on SunOS
#Work on PWS under Digital Unix.
TKLIBS=-ltk4.0 -ltcl7.4 -lX11
#Does not work on Linux with old tcl/tk installed in /usr/lib
#unless we use -static
#Does not work on PWS under Digital Unix.
#TKLIBS=-ltk -ltcl -lX11

MKF=\
  CC='$(CC)' \
  OPTS='$(OPTS)' \
  CPP='$(CPP)' \
  RANLIBTEST='$(RANLIBTEST)' \
  RANLIB='$(RANLIB)' \
  BINDIR='$(BINDIR)' \
  LIBDIR='$(LIBDIR)' \
  MANDIR='$(MANDIR)' \
  MANEXT='$(MANEXT)' \
  X11_INCLUDES='$(X11_INCLUDES)' \
  X11_LIB='$(X11_LIB)' \
  TCLINCLDIR='$(TCLINCLDIR)' \
  TKINCLDIR='$(TKINCLDIR)' \
  TKLIBDIR='$(TKLIBDIR)' \
  TCLLIBDIR='$(TCLLIBDIR)' \
  TKLIBS='$(TKLIBS)'

all:
	cd libsupport; $(MAKE) $(MKF)
	cd compiler; $(MAKE) $(MKF)
	cd lib; $(MAKE) $(MKF) all
	cd frx; $(MAKE) $(MKF)
	cd browser; $(MAKE) $(MKF)

EXDIRS=browser

examples:
	for d in $(EXDIRS); do \
	cd $$d; $(MAKE) $(MKF); cd ..; \
	done

# If Widgets.src is modified locally, then one should also do
# $(MAKE) $(MKF) depend
lib: Widgets.src
	compiler/tkcompiler
	cd lib; $(MAKE) $(MKF)

install: 
	cd lib; $(MAKE) $(MKF) install
	cd libsupport; $(MAKE) $(MKF) install
	cd compiler; $(MAKE) $(MKF) install
	cd browser; $(MAKE) $(MKF) install
#	cp -p Widgets.src $(LIBDIR)


SUBDIRS=compiler libsupport lib frx $(EXDIRS)

clean : 
	for d in $(SUBDIRS); do \
	cd $$d; $(MAKE) clean; cd ..; \
	done
