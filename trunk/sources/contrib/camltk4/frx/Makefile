CAMLC=camlc
CAMLCOMP=$(CAMLC) -c

CAMLDEP=../../../src/tools/camldep

COMPFLAGS=-W -g -I ../lib -I ../libsupport

OBJS= frx_misc.zo frx_widget.zo frx_font.zo frx_entry.zo frx_text.zo \
      frx_listbox.zo frx_req.zo frx_fillbox.zo frx_focus.zo \
      frx_dialog.zo frx_mem.zo frx_rpc.zo frx_synth.zo frx_selection.zo \
      frx_after.zo frx_fit.zo frx_ctext.zo
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
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
