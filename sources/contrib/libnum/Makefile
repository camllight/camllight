# Makefile for the "bignum" (exact rational arithmetic) library

### Configuration section

# Which version of the BigNum library to use.
# See the file bignum/README for a list of versions.
# Always safe to set TARGET_ARCH=C32, in which case the portable C version
# is used.

TARGET_ARCH=C32

# The path to the Caml Light standard library
LIBDIR=/usr/local/lib/caml-light

# Which C compiler to use.
CC=cc
CFLAGS=-O -I../../src/runtime -I./bignum/h

# Test to see whether ranlib exists on the machine
RANLIBTEST=test -f /usr/bin/ranlib -o -f /bin/ranlib

# How to invoke ranlib
RANLIB=ranlib

### End of configuration section

CAMLCOMP=camlc -c -O none
CAMLLIBR=camllibr

ZOFILES=int_misc.zo string_misc.zo nat.zo big_int.zo arith_flags.zo ratio.zo \
        num.zo num_equal.zo arith_status.zo

ZIFILES=big_int.zi nat.zi num.zi ratio.zi arith_status.zi num_equal.zi

OBJFILES=nat_stubs.o num_equal.o

all: libnums.a nums.zo $(ZIFILES)

camlnum: all
	camlmktop -o camlnum -custom $(ZOFILES) libnums.a

libnums.a: bignum/BigNum.a $(OBJFILES)
	cp bignum/BigNum.a libnums.a
	ar r libnums.a $(OBJFILES)
	if $(RANLIBTEST); then $(RANLIB) libnums.a; else true; fi

bignum/BigNum.a:
	@ cd bignum; make $(TARGET_ARCH)

install:
	cp libnums.a $(LIBDIR)/libnums.a
	if $(RANLIBTEST); then cd $(LIBDIR); $(RANLIB) libnums.a; else true; fi
	cp nums.zo $(ZIFILES) $(LIBDIR)

clean:
	rm -f libnums.a *.o *.z[io] camlnum
	@ cd bignum; make scratch
	@ cd test; make clean

nums.zo: $(ZOFILES)
	$(CAMLLIBR) -o nums.zo $(ZOFILES)

.SUFFIXES: .ml .mli .zi .zo

.mli.zi:
	$(CAMLCOMP) $<

.ml.zo:
	$(CAMLCOMP) $<

depend:
	mv Makefile Makefile.BAK; \
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.BAK; \
	 gcc -MM *.c; \
	 ../../src/tools/camldep *.mli *.ml) > Makefile

### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
big_int.zi: nat.zi
num.zi: ratio.zi nat.zi big_int.zi
ratio.zi: nat.zi big_int.zi
arith_flags.zo: arith_flags.zi
arith_status.zo: arith_status.zi arith_flags.zi
big_int.zo: big_int.zi int_misc.zi nat.zi
int_misc.zo: int_misc.zi
nat.zo: nat.zi int_misc.zi
num.zo: num.zi int_misc.zi nat.zi big_int.zi ratio.zi arith_flags.zi
num_equal.zo: num_equal.zi num.zi inum_equal.zi
ratio.zo: ratio.zi string_misc.zi int_misc.zi nat.zi big_int.zi \
    arith_flags.zi
string_misc.zo: string_misc.zi
