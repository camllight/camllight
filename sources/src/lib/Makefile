# The standard library

# The list of all interfaces. Order irrelevant.

STD_INTF= arg.zi bool.zi char.zi string.zi vect.zi eq.zi exc.zi \
    fchar.zi filename.zi float.zi fstring.zi fvect.zi hashtbl.zi \
    int.zi io.zi lexing.zi list.zi obj.zi pair.zi parsing.zi printexc.zi \
    ref.zi sort.zi sys.zi printf.zi stream.zi stack.zi queue.zi random.zi \
    genlex.zi baltree.zi set.zi map.zi gc.zi graphics.zi

# The list of all implementations. In dependency order.

STD_IMPL= bool.zo eq.zo exc.zo int.zo fstring.zo string.zo fchar.zo \
    char.zo float.zo list.zo fvect.zo vect.zo io.zo pair.zo arg.zo \
    filename.zo hashtbl.zo lexing.zo parsing.zo printexc.zo sort.zo \
    printf.zo stream.zo stack.zo queue.zo format.zo random.zo genlex.zo \
    baltree.zo set.zo map.zo gc.zo graphics.zo

GENSOURCES=filename.ml genlex.ml

all: $(STD_INTF) stdlib.zo

stdlib.zo: $(STD_IMPL)
	$(CAMLLIBR) -o stdlib.zo @&&!
$(STD_IMPL)
!

install:
	copy stdlib.zo $(LIBDIR)
	copy *.zi $(LIBDIR)
	copy *.mli $(LIBDIR)
	del $(LIBDIR)\iparsing.mli

STDLIB=yes
!include "../tools/make-ml.inc"

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
arg.zo: arg.zi eq.zi exc.zi sys.zi fstring.zi list.zi io.zi float.zi \
    fvect.zi int.zi 
baltree.zo: baltree.zi exc.zi eq.zi int.zi 
bool.zo: bool.zi 
char.zo: char.zi exc.zi eq.zi fchar.zi 
eq.zo: eq.zi 
exc.zo: exc.zi 
fchar.zo: fchar.zi fstring.zi int.zi 
filename.zi: sys.zi 
float.zo: float.zi exc.zi fstring.zi int.zi 
format.zo: format.zi eq.zi exc.zi bool.zi queue.zi fstring.zi io.zi ref.zi \
    float.zi int.zi 
format.zi: io.zi 
fstring.zo: fstring.zi eq.zi fchar.zi ref.zi int.zi 
fvect.zo: fvect.zi eq.zi list.zi int.zi 
gc.zi: io.zi 
genlex.zi: stream.zi 
hashtbl.zo: hashtbl.zi eq.zi exc.zi fvect.zi int.zi 
int.zo: int.zi eq.zi 
io.zo: io.zi exc.zi eq.zi sys.zi float.zi fstring.zi int.zi 
io.zi: sys.zi 
iparsing.zi: parsing.zi obj.zi 
lexing.zo: lexing.zi exc.zi eq.zi fstring.zi io.zi obj.zi int.zi 
lexing.zi: io.zi obj.zi 
list.zo: list.zi eq.zi exc.zi int.zi 
map.zo: map.zi baltree.zi list.zi 
map.zi: eq.zi 
pair.zo: pair.zi exc.zi 
parsing.zo: parsing.zi exc.zi eq.zi iparsing.zi fvect.zi ref.zi lexing.zi \
    obj.zi int.zi 
parsing.zi: lexing.zi obj.zi 
printexc.zo: printexc.zi eq.zi exc.zi sys.zi bool.zi io.zi fvect.zi obj.zi \
    int.zi 
printf.zo: printf.zi eq.zi exc.zi bool.zi fstring.zi io.zi obj.zi float.zi \
    int.zi 
printf.zi: io.zi 
queue.zo: queue.zi exc.zi ref.zi int.zi 
random.zo: random.zi eq.zi float.zi fvect.zi ref.zi int.zi 
set.zo: set.zi exc.zi eq.zi baltree.zi 
set.zi: eq.zi 
sort.zo: sort.zi 
stack.zo: stack.zi exc.zi list.zi 
stream.zo: stream.zi exc.zi eq.zi fstring.zi io.zi ref.zi obj.zi 
stream.zi: io.zi 
gc.zo: gc.zi printf.zi 
graphics.zo: graphics.zi eq.zi fstring.zi int.zi 
string.zo: string.zi exc.zi eq.zi sys.zi fstring.zi int.zi 
vect.zo: vect.zi exc.zi eq.zi sys.zi fvect.zi int.zi 
filename.zo: filename.zi exc.zi eq.zi fstring.zi int.zi 
genlex.zo: genlex.zi hashtbl.zi stream.zi exc.zi eq.zi fchar.zi float.zi \
    fstring.zi list.zi ref.zi int.zi 
