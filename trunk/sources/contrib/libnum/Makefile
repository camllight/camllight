# Makefile for the "bignum" (exact rational arithmetic) library

# These options are overriden when called from ../Makefile

BIGNUM_ARCH=C
CC=cc
CPP=/lib/cpp -P

# Test to see whether ranlib exists on the machine
RANLIBTEST=test -f /usr/bin/ranlib -o -f /bin/ranlib

# How to invoke ranlib
RANLIB=ranlib

# Compilation options
CFLAGS=-O -I../../src/runtime -I./bignum/h
CAMLC=camlc -g
CAMLCOMP=$(CAMLC) -c
COMPFLAGS=-W #-O fast
CAMLLIBR=camllibr
CAMLMKTOP=camlmktop

ZOFILES=int_misc.zo string_misc.zo nat.zo big_int.zo arith_flags.zo ratio.zo \
        num.zo arith_status.zo

ZIFILES=big_int.zi nat.zi num.zi ratio.zi arith_status.zi

OBJFILES=nat_stubs.o

all: libnums.a nums.zo $(ZIFILES) camlnum

camlnum: libnums.a $(ZOFILES)
	$(CAMLMKTOP) -o camlnum -custom $(ZOFILES) libnums.a

libnums.a: bignum/libbignum.a $(OBJFILES)
	cp bignum/libbignum.a libnums.a
	ar r libnums.a $(OBJFILES)
	if $(RANLIBTEST); then $(RANLIB) libnums.a; else true; fi

bignum/libbignum.a:
	cd bignum; make $(BIGNUM_ARCH) CC="$(CC)"

install:
	cp libnums.a $(LIBDIR)/libnums.a
	if $(RANLIBTEST); then cd $(LIBDIR); $(RANLIB) libnums.a; else true; fi
	cp nums.zo $(ZIFILES) $(LIBDIR)
	cp camlnum $(LIBDIR)

clean:
	rm -f libnums.a *.o *.z[io] camlnum
	rm -f nat.ml int_misc.ml
	@ cd bignum; make scratch
	@ cd test; make clean

nums.zo: $(ZOFILES)
	$(CAMLLIBR) -o nums.zo $(ZOFILES)

.SUFFIXES: .ml .mli .mlp .zi .zo

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.mlp.ml:
	@rm -f $@
	$(CPP) $< > $@
	@chmod a-w $@

int_misc.ml: int_misc.mlp
nat.ml: nat.mlp
nat.o: nat.h

depend: nat.ml int_misc.ml
	mv Makefile Makefile.BAK; \
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.BAK; \
	 ../../src/tools/camldep *.mli *.ml) > Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
big_int.zi: nat.zi 
format_latex_numbers.zi: ratio.zi num.zi nat.zi big_int.zi 
format_num.zi: ratio.zi num.zi nat.zi big_int.zi 
num.zi: ratio.zi nat.zi big_int.zi 
ratio.zi: nat.zi big_int.zi 
arith_flags.zo: arith_flags.zi 
arith_status.zo: arith_status.zi arith_flags.zi 
big_int.zo: big_int.zi int_misc.zi nat.zi 
format_latex_numbers.zo: format_latex_numbers.zi num.zi int_misc.zi nat.zi \
    big_int.zi ratio.zi 
format_num.zo: format_num.zi string_misc.zi num.zi int_misc.zi nat.zi \
    big_int.zi ratio.zi 
int_misc.zo: int_misc.zi 
nat.zo: nat.zi string_misc.zi int_misc.zi 
num.zo: num.zi int_misc.zi nat.zi big_int.zi ratio.zi arith_flags.zi 
ratio.zo: ratio.zi string_misc.zi int_misc.zi nat.zi big_int.zi \
    arith_flags.zi 
string_misc.zo: string_misc.zi 
