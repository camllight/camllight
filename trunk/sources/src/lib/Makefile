# The standard library

CAMLCOMP=../camlrun ../camlcomp
CAMLLIBR=../camlrun ../camllibr
CPP=/lib/cpp -P -Dunix
COMPFLAGS=-stdlib . -O none -g -W

# The list of all interfaces. Order irrelevant.

STD_INTF= arg.zi bool.zi char.zi string.zi vect.zi eq.zi exc.zi \
    fchar.zi filename.zi float.zi fstring.zi fvect.zi hashtbl.zi \
    int.zi io.zi lexing.zi list.zi obj.zi pair.zi parsing.zi printexc.zi \
    ref.zi sort.zi sys.zi printf.zi stream.zi stack.zi queue.zi random.zi \
    genlex.zi baltree.zi set.zi map.zi gc.zi format.zi

# The list of all implementations. In dependency order.

STD_IMPL= bool.zo eq.zo exc.zo int.zo fstring.zo string.zo fchar.zo \
    char.zo float.zo list.zo fvect.zo vect.zo io.zo pair.zo arg.zo \
    filename.zo hashtbl.zo lexing.zo parsing.zo printexc.zo sort.zo \
    printf.zo stream.zo stack.zo queue.zo format.zo random.zo genlex.zo \
    baltree.zo set.zo map.zo gc.zo

all: $(STD_INTF) stdlib.zo

stdlib.zo: $(STD_IMPL)
	$(CAMLLIBR) -o stdlib.zo $(STD_IMPL)

clean:
	rm -f *.zi *.zix *.zo
	rm -f filename.ml genlex.ml

install:
	cp stdlib.zo *.zi *.zix *.mli *.ml $(LIBDIR)
	rm $(LIBDIR)/iparsing.mli

.SUFFIXES :
.SUFFIXES : .ml .mli .zi .zo .mlp

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<
.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<
.mlp.ml:
	@rm -f $@
	$(CPP) $< > $@
	@chmod a-w $@

depend: filename.ml genlex.ml
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
         ../tools/camldep *.mli *.ml) > Makefile
	rm Makefile.bak

filename.ml: filename.mlp
genlex.ml: genlex.mlp

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
filename.zi: sys.zi 
format.zi: io.zi printf.zi 
gc.zi: io.zi 
genlex.zi: stream.zi 
io.zi: sys.zi 
iparsing.zi: obj.zi parsing.zi 
lexing.zi: io.zi obj.zi 
map.zi: eq.zi 
parsing.zi: obj.zi lexing.zi 
printf.zi: io.zi 
set.zi: eq.zi 
stream.zi: io.zi 
arg.zo: arg.zi exc.zi float.zi eq.zi fvect.zi io.zi int.zi bool.zi list.zi \
    fstring.zi sys.zi 
baltree.zo: baltree.zi exc.zi int.zi bool.zi eq.zi 
bool.zo: bool.zi 
char.zo: char.zi exc.zi fchar.zi bool.zi fstring.zi eq.zi 
eq.zo: eq.zi 
exc.zo: exc.zi 
fchar.zo: fchar.zi int.zi fstring.zi 
filename.zo: filename.zi exc.zi int.zi bool.zi fstring.zi eq.zi 
float.zo: float.zi exc.zi int.zi fstring.zi 
format.zo: format.zi io.zi exc.zi obj.zi int.zi bool.zi fstring.zi ref.zi \
    float.zi eq.zi 
fstring.zo: fstring.zi int.zi fchar.zi ref.zi eq.zi 
fvect.zo: fvect.zi int.zi list.zi eq.zi 
gc.zo: gc.zi printf.zi 
genlex.zo: genlex.zi exc.zi int.zi stream.zi fchar.zi list.zi fstring.zi \
    ref.zi hashtbl.zi float.zi eq.zi 
hashtbl.zo: hashtbl.zi fvect.zi exc.zi int.zi eq.zi 
int.zo: int.zi eq.zi 
io.zo: io.zi exc.zi int.zi bool.zi fstring.zi float.zi eq.zi sys.zi 
lexing.zo: lexing.zi io.zi exc.zi obj.zi int.zi fstring.zi eq.zi 
list.zo: list.zi exc.zi int.zi bool.zi eq.zi 
map.zo: map.zi baltree.zi 
pair.zo: pair.zi exc.zi 
parsing.zo: parsing.zi fvect.zi exc.zi obj.zi int.zi ref.zi iparsing.zi \
    eq.zi lexing.zi 
printexc.zo: printexc.zi exc.zi eq.zi fvect.zi io.zi int.zi obj.zi bool.zi \
    sys.zi 
printf.zo: printf.zi io.zi exc.zi obj.zi int.zi list.zi bool.zi ref.zi \
    fstring.zi float.zi eq.zi 
queue.zo: queue.zi exc.zi int.zi ref.zi 
random.zo: random.zi fvect.zi int.zi ref.zi eq.zi float.zi 
set.zo: set.zi exc.zi eq.zi baltree.zi 
sort.zo: sort.zi 
stack.zo: stack.zi exc.zi list.zi 
stream.zo: stream.zi exc.zi io.zi obj.zi ref.zi fstring.zi eq.zi 
string.zo: string.zi exc.zi int.zi bool.zi fstring.zi eq.zi sys.zi 
vect.zo: vect.zi fvect.zi exc.zi int.zi bool.zi eq.zi sys.zi 
