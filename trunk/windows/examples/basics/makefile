CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
CAMLDEP=perl /caml7/src/tools/camldep

all: sieve.exe fib.exe wc.exe

sieve.exe: sieve.ml
	$(CAMLC) $(LINKFLAGS) -o sieve.exe sieve.ml

fib.exe: fib.ml
	$(CAMLC) $(LINKFLAGS) -o fib.exe fib.ml

wc.exe: wc.ml
	$(CAMLC) $(LINKFLAGS) -o wc.exe wc.ml

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

