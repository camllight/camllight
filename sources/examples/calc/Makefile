CAMLC=camlc
CAMLLIBR=camllibr
CAMLLEX=camllex
CAMLYACC=camlyacc
COMPFLAGS=-W
CAMLDEP=perl /caml/src/tools/camldep

all: calc.exe

calc.exe: lexer.zo parser.zo calc.zo
	$(CAMLC) $(LINKFLAGS) -o calc.exe lexer.zo parser.zo calc.zo

parser.ml parser.mli: parser.mly
	$(CAMLYACC) parser.mly

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

clean:
	del parser.mli
	del parser.ml
	del lexer.ml
	del *.exe
	del *.zi
	del *.zo
	del *.zix

.mli.zi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.zo:
	$(CAMLC) $(COMPFLAGS) -c $<

depend: parser.ml parser.mli lexer.ml
	$(CAMLDEP) >> makefile

calc.zo: parser.zi lexer.zo 
lexer.zo: parser.zi 
parser.zo: parser.zi 
