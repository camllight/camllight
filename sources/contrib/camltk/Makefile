## Configuration
include Makefile.config

RELEASE=alpha3

all: 
	cd libsupport; $(MAKE)
	cd compiler; $(MAKE)
	compiler/tkcompiler
	cd lib; $(MAKE) depend; $(MAKE)

EXDIRS=test tktop browser

examples:
	for d in $(EXDIRS); do \
	cd $$d; $(MAKE); cd ..; \
	done
	

lib: Widgets.src
	compiler/tkcompiler
	cd lib; $(MAKE) depend; $(MAKE)


install: 
	cd lib; $(MAKE) install
	cd compiler; $(MAKE) install
	cp -p Widgets.src $(INSTALLDIR)


SUBDIRS=compiler libsupport lib $(EXDIRS)

clean : 
	for d in $(SUBDIRS); do \
	cd $$d; $(MAKE) clean; cd ..; \
	done

WORKINGFILES=\
	TODO \
	camldbx \
	graph \
	test.ml \
	tktop/Notes \
	doc/camltk \
	Makefile.config


distrib: clean
	cd ..; \
	find camltk -type d -name RCS -print >>/tmp/x$$$$; \
	find camltk \( -name '.*~' \
		-o  -name '.nfs*' \
		-o  -name '*~' \
		-o  -name '*.bak' \
		-o  -name 'TAGS' \) \
		 -print >>/tmp/x$$$$; \
	for i in $(WORKINGFILES); do echo camltk/$$i >>/tmp/x$$$$; done; \
	tar cvXf /tmp/x$$$$ /tmp/camltk-$(RELEASE).tar camltk; \
	rm -f /tmp/x$$$$
	compress /tmp/camltk-$(RELEASE).tar
	@echo "Distrib is in /tmp/camltk-$(RELEASE).tar.Z"
