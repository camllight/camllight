# WARNING: 

MKF=-f ../../Makefile.conf -f Makefile LIBDIR=$(LIBDIR) BINDIR=$(BINDIR)

all: 
	cd libsupport; $(MAKE) $(MKF)
	cd compiler; $(MAKE) $(MKF)
	compiler/tkcompiler
	cd lib; $(MAKE) -f modules $(MKF) depend; $(MAKE) -f modules $(MKF) 
	cd browser; $(MAKE) $(MKF)

EXDIRS=test tktop browser

examples:
	for d in $(EXDIRS); do \
	cd $$d; $(MAKE) $(MKF); cd ..; \
	done
	

lib: Widgets.src
	compiler/tkcompiler
	cd lib; $(MAKE) -f modules $(MKF) depend; $(MAKE) -f modules $(MKF)


install: 
	cd lib; $(MAKE) -f modules $(MKF) install
	cd libsupport; $(MAKE) $(MKF) install
	cd compiler; $(MAKE) $(MKF) install
#	cp -p Widgets.src $(LIBDIR)


SUBDIRS=compiler libsupport lib $(EXDIRS)

clean : 
	for d in $(SUBDIRS); do \
	cd $$d; $(MAKE) clean; cd ..; \
	done
