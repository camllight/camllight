CFLAGS=-g

all: texquote2 transf htmlquote

dvi2txt:
	cd dvi_to_txt; make

transf: transf.zo htmltransf.zo transfmain.zo
	camlc -o transf -g transf.zo htmltransf.zo transfmain.zo

transf.ml: transf.mll
	camllex transf.mll

htmltransf.ml: htmltransf.mll
	camllex htmltransf.mll

.SUFFIXES:
.SUFFIXES: .ml .zo .c

.ml.zo:
	camlc -c $<

.c:
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f texquote2 transf
	rm -f transf.ml
	rm -f *.o *.zi *.zo *.zix
	rm -f *~ #*#
	cd dvi_to_txt; make clean
