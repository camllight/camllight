# WARNING: 
# These are for local compilation. They are overriden by Makefile
# in cl/contrib
CC=gcc
OPTS=
CPP=/lib/cpp -P
RANLIBTEST=test -f /usr/bin/ranlib -o -f /bin/ranlib
BINDIR=${HOME}/mycl/bin
LIBDIR=${HOME}/mycl
RANLIB=ranlib
MANEXT=1
MANDIR=/usr/local/man/man$(MANEXT)
X11_INCLUDES=/usr/include
X11_LIB=/usr/X11R6/lib
TCLINCLDIR=/usr/local/lib/tcl7.4/include
TKINCLDIR=/usr/local/lib/tk4.0/include
TKLIBDIR=/usr/local/lib/tk4.0/lib
TCLLIBDIR=/usr/local/lib/tcl7.4/lib
#Does not work on SunOS
#TKLIBS=-ltk4.0 -ltcl7.4 -lX11
#Does not work on Linux with tcl/tk installed in /usr/lib
TKLIBS=-ltk -ltcl -lX11

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

LIBMKF=-f modules -f Makefile $(MKF)

all:
	cd libsupport; $(MAKE) $(MKF)
	cd compiler; $(MAKE) $(MKF)
	cd lib; $(MAKE) $(LIBMKF) all
	cd frx; $(MAKE) $(MKF)
	cd browser; $(MAKE) $(MKF)

EXDIRS=browser

examples:
	for d in $(EXDIRS); do \
	cd $$d; $(MAKE) $(MKF); cd ..; \
	done
	

# If Widgets.src is modified locally, then one should also do
# $(MAKE) $(LIBMKF) depend
lib: Widgets.src
	compiler/tkcompiler
	cd lib; $(MAKE) $(LIBMKF)

install: 
	cd lib; $(MAKE) $(LIBMKF) install
	cd libsupport; $(MAKE) $(MKF) install
	cd compiler; $(MAKE) $(MKF) install
	cd browser; $(MAKE) $(MKF) install
#	cp -p Widgets.src $(LIBDIR)


SUBDIRS=compiler libsupport lib frx $(EXDIRS)

clean : 
	for d in $(SUBDIRS); do \
	cd $$d; $(MAKE) clean; cd ..; \
	done
