CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
CAMLDEP=perl /caml7/src/tools/camldep

all: kb.exe

kb.exe: prelude.zo terms.zo equation.zo order.zo kb.zo go.zo
	$(CAMLC) $(LINKFLAGS) -o kb.exe prelude.zo terms.zo equation.zo order.zo kb.zo go.zo

clean:
	del *.exe
	del *.zi
	del *.zo
	del *.zix

.SUFFIXES: .ml .mli .zo .zi

.mli.zi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.zo:
	$(CAMLC) $(COMPFLAGS) -c $<

depend:
	$(CAMLDEP) >> makefile

equation.zo: prelude.zo terms.zo 
go.zo: order.zo kb.zo terms.zo 
kb.zo: equation.zo prelude.zo terms.zo 
order.zo: terms.zo 
gotop.zo: order.zo kb.zo terms.zo 
