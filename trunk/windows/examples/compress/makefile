CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
CAMLDEP=perl /caml7/src/tools/camldep

all: compr.exe

compr.exe: esbit.zo fileprio.zo huffman.zo compr.zo
	$(CAMLC) $(LINKFLAGS) -o compr.exe esbit.zo fileprio.zo huffman.zo compr.zo

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

compr.zo: huffman.zi 
decompr.zo: huffman.zi 
esbit.zo: esbit.zi 
fileprio.zo: fileprio.zi 
huffman.zo: huffman.zi esbit.zi fileprio.zi 
loadall.zo: compr.zo 
