# Makefile for the "bignum" (exact rational arithmetic) library

# These options are overriden when called from ../Makefile

BIGNUM_ARCH=C
CC=cc

# Test to see whether ranlib exists on the machine
RANLIBTEST=test -f /usr/bin/ranlib -o -f /bin/ranlib

# How to invoke ranlib
RANLIB=ranlib

# Compilation options
CFLAGS=-O -I../../src/runtime -I./bignum/h
CAMLC=camlc
CAMLCOMP=$(CAMLC) -c
COMPFLAGS=-W #-O fast
CAMLLIBR=camllibr
CAMLMKTOP=camlmktop

ZOFILES=int_misc.zo fnat.zo nat.zo big_int.zo arith_flags.zo ratio.zo \
        num.zo arith_status.zo

ZOFILESD=${ZOFILES} ltx_num.zo fmt_num.zo

ZIFILES=big_int.zi fnat.zi nat.zi num.zi ratio.zi arith_status.zi
ZIFILESD=$(ZIFILES) ltx_num.zi fmt_num.zi

OBJFILES=nat_stubs.o

all: libnums.a nums.zo $(ZIFILESD) camlnum ${ZOFILESD} ${ZIFILESD}

camlnum: libnums.a $(ZOFILES) numprint.zo
	$(CAMLMKTOP) -o camlnum -custom $(ZOFILES) numprint.zo libnums.a

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
	@ cd bignum; make scratch
	@ cd test; make clean

nums.zo: $(ZOFILESD)
	$(CAMLLIBR) -o nums.zo $(ZOFILES)

.SUFFIXES: .ml .mli .zi .zo

.mli.zi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.zo:
	$(CAMLCOMP) $(COMPFLAGS) $<

nat.o: nat.h

depend:
	mv Makefile Makefile.BAK; \
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.BAK; \
	 ../../src/tools/camldep *.mli *.ml) > Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
big_int.zi: nat.zi 
fmt_num.zi: ratio.zi num.zi nat.zi big_int.zi 
ltx_num.zi: ratio.zi num.zi nat.zi big_int.zi 
nat.zi: fnat.zi 
num.zi: ratio.zi nat.zi big_int.zi 
ratio.zi: nat.zi big_int.zi 
arith_flags.zo: arith_flags.zi 
arith_status.zo: arith_status.zi arith_flags.zi 
big_int.zo: big_int.zi int_misc.zi nat.zi fnat.zi 
fmt_num.zo: fmt_num.zi ratio.zi num.zi int_misc.zi nat.zi big_int.zi 
fnat.zo: fnat.zi int_misc.zi 
int_misc.zo: int_misc.zi 
ltx_num.zo: ltx_num.zi num.zi int_misc.zi nat.zi big_int.zi ratio.zi 
nat.zo: nat.zi fnat.zi 
num.zo: num.zi int_misc.zi big_int.zi ratio.zi fnat.zi arith_flags.zi 
numprint.zo: ratio.zi num.zi nat.zi big_int.zi 
ratio.zo: ratio.zi int_misc.zi big_int.zi fnat.zi arith_flags.zi 
