CAMLC=camlc
CAMLCOMP=$(CAMLC) -c

CAMLDEP=camldep

COMPFLAGS=-W -g -I ../lib -I ../libsupport

OBJS= frx_widget.zo frx_font.zo frx_entry.zo frx_text.zo \
      frx_listbox.zo frx_req.zo frx_fillbox.zo frx_focus.zo
UNUSED=frx_toplevel.zo frx_lbutton.zo

all: libfrx.zo

libfrx.zo: $(OBJS)
	camllibr -o libfrx.zo $(OBJS)

clean:
	rm -f *.z*

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
frx_req.zo: frx_listbox.zo frx_widget.zi 
frx_text.zo: frx_entry.zo 
frx_toplevel.zo: frx_toplevel.zi 
frx_widget.zo: frx_widget.zi 
