CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
CAMLDEP=perl /caml386/src/tools/camldep

all: kb.exe

kb.exe: prelude.zo terms.zo equation.zo order.zo kb.zo go.zo
	$(CAMLC) $(LINKFLAGS) -o kb.exe prelude.zo terms.zo equation.zo order.zo kb.zo go.zo

clean:
	del *.exe
	del *.zi
	del *.zo
	del *.zix

.mli.zi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.zo:
	$(CAMLC) $(COMPFLAGS) -c $<

depend:
	$(CAMLDEP) >> makefile

kb.zo: equation.zo prelude.zo terms.zo 
terms.zo: prelude.zo 
go.zo: order.zo equation.zo prelude.zo kb.zo terms.zo 
equation.zo: prelude.zo terms.zo 
order.zo: prelude.zo terms.zo 
