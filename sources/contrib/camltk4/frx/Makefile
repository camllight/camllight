PROFILING=${CAMLPROF}
CAMLC=camlc
CAMLCOMP=$(CAMLC) -c $(PROFILING)
CPP=/lib/cpp -P -Dunix

CAMLYACC=camlyacc -v
CAMLLEX=camllex

CAMLDEP=camldep

X11_LIB=/usr/lib
TCLLIBDIR=/usr/local/lib/tcl/lib
TKLIBDIR=/usr/local/lib/tk/lib
TKLIBS=-ltk -ltcl -lX11

TKLINKOPT=-ccopt -L$(TCLLIBDIR) -ccopt -L$(TKLIBDIR) -ccopt -L$(X11_LIB) \
	   -lcaml -lcamltk $(TKLIBS)

# Use 4.0
COMPFLAGS=-W -g -I ../lib
LINKFLAGS= -custom $(PROFILING)

OBJS= frx_widget.zo frx_font.zo frx_entry.zo frx_text.zo \
      frx_listbox.zo frx_req.zo frx_toplevel.zo frx_lbutton.zo \
      frx_fillbox.zo frx_focus.zo


all: libfrx.zo

libfrx.zo: $(OBJS)
	camllibr -o libfrx.zo $(OBJS)

clean:
	rm *.z*

.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<


depend: 
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
	 $(CAMLDEP) *.mli *.ml) > Makefile


### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
frx_font.zo: frx_font.zi 
frx_lbutton.zo: frx_lbutton.zi 
frx_req.zo: frx_listbox.zo frx_entry.zo frx_widget.zi 
frx_toplevel.zo: frx_toplevel.zi 
frx_widget.zo: frx_widget.zi 
