CFLAGS=-g

all: texquote2 transf

texquote2: texquote2.c
	$(CC) $(CFLAGS) -o texquote2 texquote2.c

dvi2txt:
	cd dvi_to_txt; make

transf: transf.zo transfmain.zo
	camlc -o transf transf.zo transfmain.zo

transf.ml: transf.mll
	camllex transf.mll

.SUFFIXES:
.SUFFIXES: .ml .zo

.ml.zo:
	camlc -c $<

clean:
	rm -f texquote2 transf
	rm -f transf.ml
	rm -f *.o *.zi *.zo *.zix
	rm -f *~ #*#
	cd dvi_to_txt; make clean
