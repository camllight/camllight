CAMLCOMP=../camlrun ../camlcomp -stdlib ../lib
CAMLLINK=../camlrun ../camllink -stdlib ../lib
INCLUDES=-I ../compiler -I ../linker
COMPFLAGS=-O fast $(INCLUDES)
LINKFLAGS=-g $(INCLUDES)

all: dumpsymb dumpobj

dumpsymb: dumpsymb.zo
	$(CAMLLINK) $(LINKFLAGS) -o dumpsymb stdlib.zo dumpsymb.zo
	
dumpobj: opnames.zo dumpobj.zo
	$(CAMLLINK) $(LINKFLAGS) -o dumpobj stdlib.zo opcodes.zo opnames.zo dumpobj.zo

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

clean:
	rm -f *.zo *.zi dumpobj dumpsymb

depend:
