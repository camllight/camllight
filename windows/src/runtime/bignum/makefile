CC = cl /nologo
LIB = bignum.lib
OBJECT = o\KerN.obj o\bnInit.obj o\bnMult.obj o\bnDivide.obj o\bnCmp.obj o\bzf.obj o\bz.obj 
KERNH = h\BigNum.h 
CFLAGS = -c -I.\h -Ox -DCAML_LIGHT

all: $(LIB)

scratch:
	del o\*.obj
	del libbignum.lib

# build the BigNum library
$(LIB): $(OBJECT)
	lib /out:$(LIB) $(OBJECT)

o\KerN.obj: c\KerN.c 
	$(CC) $(CFLAGS) c\KerN.c
	mv KerN.obj o

o\bnInit.obj: c\bn\bnInit.c $(KERNH)
	$(CC) $(CFLAGS) c\bn\bnInit.c
	mv bnInit.obj o

o\bnMult.obj: c\bn\bnMult.c $(KERNH)
	$(CC) $(CFLAGS) c\bn\bnMult.c
	mv bnMult.obj o

o\bnDivide.obj: c\bn\bnDivide.c $(KERNH)
	$(CC) $(CFLAGS) c\bn\bnDivide.c
	mv bnDivide.obj o

o\bnCmp.obj: c\bn\bnCmp.c $(KERNH)
	$(CC) $(CFLAGS) c\bn\bnCmp.c
	mv bnCmp.obj o

o\bz.obj: c\bz.c h/BigZ.h $(KERNH)
	$(CC) $(CFLAGS) c\bz.c
	mv bz.obj o

o\br.obj: c\br.c h/BigR.h h/BigZ.h $(KERNH)
	$(CC) $(CFLAGS) c\br.c
	mv br.obj o

o\bzf.obj: c\bzf.c h/BigZ.h $(KERNH)
	$(CC) $(CFLAGS) c\bzf.c
	mv bzf.obj o
