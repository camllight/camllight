# WARNING: 

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
	cd browser; $(MAKE) $(MKF)

EXDIRS=test browser

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


SUBDIRS=compiler libsupport lib $(EXDIRS)

clean : 
	for d in $(SUBDIRS); do \
	cd $$d; $(MAKE) clean; cd ..; \
	done
