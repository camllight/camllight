CAMLTKDIR=../lib
SUPPORTDIR=../libsupport
CAMLC=camlc
CAMLCOMP=$(CAMLC) -c
CPP=/lib/cpp -P -Dunix
COMPFLAGS=-g -O fast -I $(CAMLTKDIR)
LINKFLAGS=-g -custom
CAMLYACC=camlyacc -v
CAMLLEX=camllex
CAMLDEP=../../../src/tools/camldep

TKLINKOPT=-ccopt -L$(TCLLIBDIR) -ccopt -L$(TKLIBDIR) -ccopt -L$(X11_LIB) \
	  -ccopt -L$(SUPPORTDIR) -lcaml -lcamltk $(TKLIBS)


# Programs in the two-process architecture
PGM2=	addition.two \
	bindtest.two \
	buttons.two \
	calc.two \
	canvastest.two \
	entryparams.two \
	lbox.two \
	menutest.two \
	text1.two \
	text2.two \
	tkmesbox.two \
	helloworld.two

# Programs in the single-process architecture
PGM1=  addition.one \
	bindtest.one \
	buttons.one \
	calc.one \
	canvastest.one \
	entryparams.one \
	lbox.one \
	menutest.one \
	text1.one \
	text2.one \
	tkmesbox.one \
        helloworld.one

all: $(PGM2) $(PGM1)

clean : 
	rm -f *.z? $(PGM2) $(PGM1)


.SUFFIXES :
.SUFFIXES : .mli .ml .zi .zo .mlp .two .one

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.zo.one:
	$(CAMLC) $(LINKFLAGS) -I $(CAMLTKDIR) -o $@ \
		tklib.zo $< $(TKLINKOPT)

.zo.two:
	$(CAMLC) $(LINKFLAGS) -I $(CAMLTKDIR) -o $@ unix.zo \
		tklib2.zo $< -lunix



addition.one: addition.zo
bindtest.one: bindtest.zo
buttons.one: buttons.zo
calc.one: calc.zo
canvastest.one: canvastest.zo
entryparams.one: entryparams.zo
lbox.one: lbox.zo
menutest.one: menutest.zo
text1.one: text1.zo
text2.one: text2.zo
tkmesbox.one: tkmesbox.zo
helloworld.one: helloworld.zo
addition.two: addition.zo
bindtest.two: bindtest.zo
buttons.two: buttons.zo
calc.two: calc.zo
canvastest.two: canvastest.zo
entryparams.two: entryparams.zo
lbox.two: lbox.zo
menutest.two: menutest.zo
text1.two: text1.zo
text2.two: text2.zo
tkmesbox.two: tkmesbox.zo
helloworld.two: helloworld.zo
