PROFILING=${CAMLPROF}
SUPPORTDIR=../libsupport
UNIXDIR=../../libunix
CAMLC=camlc
CAMLCOMP=$(CAMLC) -c
CPP=/lib/cpp -P -Dunix
COMPFLAGS=-W -O fast -g -I $(UNIXDIR) -I $(SUPPORTDIR) $(PROFILING)
LINKFLAGS=-g $(PROFILING)
CAMLYACC=camlyacc -v
CAMLLEX=camllex
CAMLDEP=../../../src/tools/camldep


X11_LIB=/usr/X11R6/lib
TCLLIBDIR=/usr/local/lib/tcl/lib
TKLIBDIR=/usr/local/lib/tk/lib
TKLIBS=-ltcl -ltk -lX11

TKLINKOPT=-ccopt -L$(TCLLIBDIR) -ccopt -L$(TKLIBDIR) -ccopt -L$(X11_LIB) \
	   $(SUPPORTDIR)/camltk.o -lcaml $(TKLIBS)

SUPPORT=../libsupport/support.zo ../libsupport/protocol.zo \
   ../libsupport/textvariable.zo

# two-process protocol not supported anymore
all : tklib.zo toplevels

toplevels: camltktop

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

tklib.zo : $(SUPPORT) $(WIDGETOBJS) tk.zo 
	camllibr -o tklib.zo $(SUPPORT) tk.zo $(WIDGETOBJS)

camltktop : $(WIDGETOBJS) $(SUPPORT)
	camlmktop -o camltktop -I $(SUPPORTDIR) $(PROFILING) \
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
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
	 $(CAMLDEP) *.mli *.ml) > Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
button.zi: tk.zo 
canvas.zi: tk.zo 
checkbutton.zi: tk.zo 
colormodel.zi: tk.zo 
contrib.zi: tk.zo 
entry.zi: tk.zo 
focus.zi: tk.zo 
frame.zi: tk.zo 
grab.zi: tk.zo 
label.zi: tk.zo 
listbox.zi: tk.zo 
menu.zi: tk.zo 
menubar.zi: tk.zo 
menubutton.zi: tk.zo 
message.zi: tk.zo 
pack.zi: tk.zo 
place.zi: tk.zo 
radiobutton.zi: tk.zo 
resource.zi: tk.zo 
scale.zi: tk.zo 
scrollbar.zi: tk.zo 
selection.zi: tk.zo 
text.zi: tk.zo 
toplevelw.zi: tk.zo 
wait.zi: tk.zo 
winfo.zi: tk.zo 
wm.zi: tk.zo 
button.zo: button.zi tk.zo 
canvas.zo: canvas.zi tk.zo 
checkbutton.zo: checkbutton.zi tk.zo 
colormodel.zo: colormodel.zi tk.zo 
contrib.zo: contrib.zi tk.zo 
entry.zo: entry.zi tk.zo 
focus.zo: focus.zi tk.zo 
frame.zo: frame.zi tk.zo 
grab.zo: grab.zi tk.zo 
label.zo: label.zi tk.zo 
listbox.zo: listbox.zi tk.zo 
menu.zo: menu.zi tk.zo 
menubar.zo: menubar.zi tk.zo 
menubutton.zo: menubutton.zi tk.zo 
message.zo: message.zi tk.zo 
pack.zo: pack.zi tk.zo 
place.zo: place.zi tk.zo 
radiobutton.zo: radiobutton.zi tk.zo 
resource.zo: resource.zi tk.zo 
scale.zo: scale.zi tk.zo 
scrollbar.zo: scrollbar.zi tk.zo 
selection.zo: selection.zi tk.zo 
text.zo: text.zi tk.zo 
tkgen.zo: tkgen.zi 
toplevelw.zo: toplevelw.zi tk.zo 
wait.zo: wait.zi tk.zo 
winfo.zo: winfo.zi tk.zo 
wm.zo: wm.zi tk.zo 
