CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
COMPFLAGS=-W
CAMLDEP=perl /caml/src/tools/camldep

all: asl.exe

asl.exe: asl.zo token.zo parser.zo semant.zo typing.zo main.zo run.zo
	$(CAMLC) $(LINKFLAGS) -o asl.exe asl.zo token.zo parser.zo semant.zo typing.zo main.zo run.zo

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

asl.zo: asl.zi 
load.zo: token.zi typing.zo parser.zi semant.zo main.zi 
loadall.zo: main.zi 
main.zo: main.zi token.zi typing.zo parser.zi semant.zo asl.zi 
parser.zo: parser.zi token.zi asl.zi 
parser.zi: token.zi 
run.zo: main.zi 
semant.zo: parser.zi 
token.zo: token.zi 
typing.zo: parser.zi 
