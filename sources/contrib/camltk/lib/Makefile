SUPPORTDIR=../libsupport

CAMLC=camlc
CAMLCOMP=$(CAMLC) -c
CPP=/lib/cpp -P -Dunix
COMPFLAGS=-O fast -g -I $(SUPPORTDIR)
LINKFLAGS=-g
CAMLYACC=camlyacc -v
CAMLLEX=camllex
CAMLDEP=../../../src/tools/camldep

TKLINKOPT=-ccopt -L$(TCLLIBDIR) -ccopt -L$(TKLIBDIR) \
	   $(SUPPORTDIR)/camltk.o -lcaml -ltk -ltcl -lX11


SUPPORT1=$(SUPPORTDIR)/support.zo $(SUPPORTDIR)/protocol1.zo \
         $(SUPPORTDIR)/textvariable.zo

SUPPORT2=$(SUPPORTDIR)/support.zo $(SUPPORTDIR)/protocol2.zo \
         $(SUPPORTDIR)/textvariable.zo

SUPPORT=support.zo protocol.zo textvariable.zo

all : $(WIDGETOBJS) tklib1.zo tklib2.zo camltktop1 camltktop2

toplevels: camltktop1 camltktop2

tklib1.zo : $(SUPPORT1) $(WIDGETOBJS)
	camllibr -o tklib1.zo $(SUPPORT1) tk.zo $(WIDGETOBJS)

tklib2.zo : $(SUPPORT2) $(WIDGETOBJS)
	camllibr -o tklib2.zo $(SUPPORT2) tk.zo $(WIDGETOBJS)

camltktop1 : $(WIDGETOBJS) $(SUPPORT1)
	cp -p $(SUPPORTDIR)/protocol1.zo $(SUPPORTDIR)/protocol.zo
	camlmktop -o camltktop1 -I $(SUPPORTDIR) \
	          -custom $(SUPPORT) tk.zo $(WIDGETOBJS) \
		                $(TKLINKOPT)


camltktop2 : $(WIDGETOBJS) $(SUPPORT2)
	cp -p $(SUPPORTDIR)/protocol2.zo $(SUPPORTDIR)/protocol.zo
	camlmktop -o camltktop2 -I $(SUPPORTDIR) \
	            -custom unix.zo $(SUPPORT) tk.zo $(WIDGETOBJS) -lunix


clean : 
	rm -f *.z* *.ml *.mli tklib[12].zo  camltktop[12]

install: tklib1.zo tklib2.zo  camltktop1 camltktop2
	[ -d $(LIBDIR) ] || mkdir $(LIBDIR)
	cp -p *.zi tklib?.zo camltktop? $(LIBDIR)
	chmod 644 $(LIBDIR)/*.zi
	chmod 644 $(LIBDIR)/tklib?.zo
	chmod 755 $(LIBDIR)/camltktop?

tk.ml : tkgen.ml $(SUPPORTDIR)/report.ml $(SUPPORTDIR)/*.ml
	(echo '#open "support";;'; \
	 echo '#open "protocol";;'; \
	 echo '#open "textvariable";;'; \
	 cat $(SUPPORTDIR)/report.ml; \
	 cat $(SUPPORTDIR)/builtin_*.ml; \
	 cat tkgen.ml; \
         cat $(SUPPORTDIR)/builtina_*.ml) > tk.ml

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
