PROGS=client.out compose.out copy_rec.out file_copy.out grepi.out leave.out \
      multiplex.out server.out shell.out sieve.out

all: $(PROGS)

clean scratch:
	rm -f *.out *.z[io]

.SUFFIXES: .ml .out

.ml.out:
	camlc -o $@ -custom  -I .. unix.zo $< ../libunix.a

