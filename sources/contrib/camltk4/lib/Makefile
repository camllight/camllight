SUPPORTDIR=../libsupport
UNIXDIR=../../libunix
CAMLC=camlc
CAMLCOMP=$(CAMLC) -c
CPP=/lib/cpp -P -Dunix
COMPFLAGS=-W -O fast -g -I $(UNIXDIR) -I $(SUPPORTDIR)
LINKFLAGS=-g
CAMLYACC=camlyacc -v
CAMLLEX=camllex
CAMLDEP=../../../src/tools/camldep

LIBDIR=/usr/local/lib/caml-light

X11_LIB=/usr/X11R6/lib
TCLLIBDIR=/usr/local/lib/tcl7.4/lib
TKLIBDIR=/usr/local/lib/tk4.0/lib
TKLIBS=-ltk -ltcl -lX11

TKLINKOPT=-ccopt -L$(TCLLIBDIR) -ccopt -L$(TKLIBDIR) -ccopt -L$(X11_LIB) \
	   $(SUPPORTDIR)/camltk.o $(TKLIBS)

SUPPORT=../libsupport/catchexc.zo \
        ../libsupport/hashtblc.zo \
        ../libsupport/support.zo ../libsupport/protocol.zo \
        ../libsupport/textvariable.zo \
	../libsupport/optionmenu.zo

EXTRAS=	../libsupport/optionmenu.zo \
	../libsupport/dialog.zo


all : tklib.zo toplevels

toplevels: camltktop

include ./modules

tkgen.ml: ../Widgets.src
	cd ..; compiler/tkcompiler

tk.ml : tkgen.ml $(SUPPORTDIR)/report.ml $(SUPPORTDIR)/builtin_*.ml
	(echo '#open "support";;'; \
	 echo '#open "protocol";;'; \
	 echo '#open "textvariable";;'; \
	 cat $(SUPPORTDIR)/report.ml; \
	 cat $(SUPPORTDIR)/builtin_*.ml; \
	 cat tkgen.ml; \
         cat $(SUPPORTDIR)/builtina_*.ml) > tk.ml

tklib.zo : $(SUPPORT) $(WIDGETOBJS) $(EXTRAS) tk.zo 
	camllibr -o tklib.zo $(SUPPORT) tk.zo $(WIDGETOBJS) $(EXTRAS)

camltktop : $(WIDGETOBJS) $(SUPPORT)
	camlmktop -o camltktop -I $(SUPPORTDIR) \
	          -custom $(SUPPORT) tk.zo $(WIDGETOBJS) \
		                $(TKLINKOPT)

clean : 
	rm -f *.z* *.ml *.mli tklib.zo  camltktop

install: tklib.zo camltktop
	if test -d $(LIBDIR); then : ; else mkdir $(LIBDIR); fi
	cp *.zi tklib.zo camltktop $(LIBDIR)
	chmod 644 $(LIBDIR)/*.zi
	chmod 644 $(LIBDIR)/tklib.zo
	chmod 755 $(LIBDIR)/camltktop


.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo .mlp

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

depend: tk.ml
	$(CAMLDEP) *.mli *.ml > .depend

../libsupport/dialog.zo: ../libsupport/dialog.zi tk.zi

include .depend
