CAMLCOMP=../camlrun ../camlcomp -W -stdlib ../lib
CAMLLINK=../camlrun ../camllink -stdlib ../lib
CAMLLEX=../camlrun ../camllex
INCLUDES=-I ../compiler -I ../linker
COMPFLAGS=-O fast $(INCLUDES)
LINKFLAGS=-g $(INCLUDES)

all: dumpsymb dumpobj dumpintf findmsgs clprepro

clprepro: clprepro.c
	cc clprepro.c -o clprepro

dumpsymb: dumpsymb.zo
	$(CAMLLINK) $(LINKFLAGS) -o dumpsymb stdlib.zo dumpsymb.zo
	
dumpobj: opnames.zo dumpobj.zo
	$(CAMLLINK) $(LINKFLAGS) -o dumpobj stdlib.zo opcodes.zo opnames.zo dumpobj.zo

dumpintf: dumpintf.zo
	$(CAMLLINK) $(LINKFLAGS) -o dumpintf stdlib.zo config.zo misc.zo const.zo globals.zo modules.zo builtins.zo types.zo pr_type.zo dumpintf.zo

findmsgs: scanmsgs.zo findmsgs.zo
	$(CAMLLINK) $(LINKFLAGS) -o findmsgs stdlib.zo scanmsgs.zo findmsgs.zo

.SUFFIXES:
.SUFFIXES:
.SUFFIXES: .ml .zo

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

opnames.ml: ../runtime/instruct.h
	sed -e '/\/\*/d'                                                      \
            -e 's/enum \(.*\) {/let names_of_\1 = [|/'                        \
            -e 's/};$$/|];;/'                                                 \
            -e 's/\([A-Z][A-Z_0-9]*\)/"\1"/g'                                 \
            -e 's/,/;/g'                                                      \
        ../runtime/instruct.h > opnames.ml

scanmsgs.ml: scanmsgs.mll
	$(CAMLLEX) scanmsgs.mll

clean:
	rm -f *.zo *.zi dumpobj dumpsymb opnames.ml findmsgs scanmsgs.ml

depend:
