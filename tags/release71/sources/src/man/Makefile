MAN=camlc.m camllex.m camllibr.m camllight.m camlmktop.m camlrun.m camlyacc.m
MANEXT=1
MANDIR=/usr/man/man$(MANEXT)

install:
	for f in $(MAN); do cp $$f $(MANDIR)/`basename $$f .m`.$(MANEXT); done
