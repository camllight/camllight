#  Copyright     Digital Equipment Corporation & INRIA     1988, 1989
#  Adapted by Xavier Leroy on Fri Apr 30 1993
#  Last modified on Fri May 31 18:26:52 GMT+2:00 1991 by herve 
#       modified on Fri Mar  1 18:32:21 GMT+1:00 1991 by shand 
#       modified on Thu Nov  2 14:23:14 GMT+1:00 1989 by gangnet 
#       modified on Wed Jul  5 10:23:54 GMT+2:00 1989 by bertin 

CC = cc
LIB = BigNum.a
OBJECT = o/KerN.o o/bnInit.o o/bnMult.o o/bnDivide.o o/bnCmp.o o/bzf.o o/bz.o 
KERNH = h/BigNum.h 
CFLAGS = -c -I./h -O -DCAML_LIGHT
LKFLAGS =

default:
	@echo "Usage: make <version>"
	@echo "see README for valid versions."
	@sh -c 'exit 1'

all: $(LIB)

tidy:
	-rm -f ,* .,* *~ #*# .emacs_[0-9]* *.BAK *.CKP core a.out
	-rm -f */,* */.,* */*~ */#*# */.emacs_[0-9]* */*.BAK */*.CKP

scratch: tidy 
	rm -f $(LIB) o/*.o testKerN bztest brtest

# build the BigNum library
$(LIB): $(OBJECT)
	-rm -f $(LIB)
	ar cr $(LIB) $(OBJECT)
	ranlib $(LIB)

# How to choose the machine dependant version. All produce KerN.o
o/KerN.o: c/KerN.c 
	@echo "The Default is KerN written in C with digits on 32 bits"
	make C32 CC=$(CC) CFLAGS="$(CFLAGS)"

C16: scratch
	$(CC) $(CFLAGS) -DDIGITon16BITS c/KerN.c
	mv KerN.o o
	make CC=$(CC) CFLAGS="$(CFLAGS) -DDIGITon16BITS" \
	     OBJECT="$(OBJECT) o/bnMult.o" all

C32: scratch
	$(CC) $(CFLAGS) -DDIGITon32BITS c/KerN.c
	mv KerN.o o
	make CC=$(CC) CFLAGS="$(CFLAGS) -DDIGITon32BITS" \
	     OBJECT="$(OBJECT) o/bnMult.o" all

68K: scratch
	as s/68KerN.s -o o/KerN.o
	make CC=$(CC) CFLAGS="$(CFLAGS) -DDIGITon32BITS" \
	     OBJECT="$(OBJECT) o/bnMult.o" all

vax: scratch
	as s/vaxKerN.s -o o/KerN.o
	make CC=$(CC) CFLAGS="$(CFLAGS) -DDIGITon32BITS" all

ns: scratch
	as s/nsKerN.s -o o/KerN.o
	make CC=$(CC) CFLAGS="$(CFLAGS) -DDIGITon32BITS" \
	     OBJECT="$(OBJECT) o/bnMult.o" all

mips: scratch
	as -w s/mipsKerN.s -o o/KerN.o
	make CC=$(CC) CFLAGS="$(CFLAGS) -DDIGITon32BITS" all

pyramid: scratch
	as s/pyramidKerN.s -o o/KerN.o
	make CC=$(CC) CFLAGS="$(CFLAGS) -DDIGITon32BITS" \
	     OBJECT="$(OBJECT) o/bnMult.o" all

i960: scratch
	as s/i960KerN.s -o o/KerN.o
	make CC=$(CC) CFLAGS="$(CFLAGS) -DDIGITon32BITS" \
	     OBJECT="$(OBJECT) o/bnMult.o" all

sparc: scratch
	as s/sparcKerN.s -o o/KerN.o
	make CC=$(CC) CFLAGS="$(CFLAGS) -DDIGITon32BITS" \
	     OBJECT="$(OBJECT) o/bnMult.o" all

sparcfpu: scratch
	as s/sparcfpuKerN.s -o o/KerN.o
	make CC=$(CC) CFLAGS="$(CFLAGS) -DDIGITon32BITS" \
	     OBJECT="$(OBJECT) o/bnMult.o" all

rs6000: scratch
	as s/RS6000KerN.s -o o/KerN.o
	make CC=$(CC) CFLAGS="$(CFLAGS) -DDIGITon32BITS" \
	     OBJECT="$(OBJECT) o/bnMult.o" all

# Construct VMS assembler from UNIX version

s/vaxKerN.mar: s/vaxKerN.s
	sed -f s/unix2vms.sed < s/vaxKerN.s > $@

# Level N
o/bnInit.o: c/bn/bnInit.c $(KERNH)
	$(CC) $(CFLAGS) c/bn/bnInit.c
	mv bnInit.o o

o/bnMult.o: c/bn/bnMult.c $(KERNH)
	$(CC) $(CFLAGS) c/bn/bnMult.c
	mv bnMult.o o

o/bnDivide.o: c/bn/bnDivide.c $(KERNH)
	$(CC) $(CFLAGS) c/bn/bnDivide.c
	mv bnDivide.o o

o/bnCmp.o: c/bn/bnCmp.c $(KERNH)
	$(CC) $(CFLAGS) c/bn/bnCmp.c
	mv bnCmp.o o

# Level Z
o/bz.o: c/bz.c h/BigZ.h $(KERNH)
	$(CC) $(CFLAGS) c/bz.c
	mv bz.o o

# level R
o/br.o: c/br.c h/BigR.h h/BigZ.h $(KERNH)
	$(CC) $(CFLAGS) c/br.c
	mv br.o o

# Some functions build with BigZ
o/bzf.o: c/bzf.c h/BigZ.h $(KERNH)
	$(CC) $(CFLAGS) c/bzf.c
	mv bzf.o o

# Tests Of KerN
testKerN: o/testKerN.o $(LIB)
	$(CC) o/testKerN.o $(LIB) $(LKFLAGS) -o testKerN

o/testKerN.o: c/testKerN.c $(KERNH) h/BntoBnn.h
	$(CC) $(CFLAGS) c/testKerN.c
	mv testKerN.o o

# Tests Of BigZ
bztest: o/bztest.o $(LIB)
	$(CC) o/bztest.o $(LIB) $(LKFLAGS) -o bztest

o/bztest.o: c/bztest.c h/BigZ.h $(KERNH)
	$(CC) $(CFLAGS) c/bztest.c
	mv bztest.o o

# Tests Of BigR
brtest: o/brtest.o $(LIB)
	$(CC) o/brtest.o $(LIB) $(LKFLAGS) -o brtest

o/brtest.o: c/brtest.c h/BigR.h h/BigZ.h $(KERNH)
	$(CC) $(CFLAGS) c/brtest.c
	mv brtest.o o

