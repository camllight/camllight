# Common make definitions for C programs

clean:
	rm -f $(OUTPUT) $(GENSOURCES)
	rm -f *.obj *.lib
	rm -f *.o *.a

.c.o:
	$(CC) $(CFLAGS) -c $<


depend: $(GENSOURCES)
	mv makefile makefile.bak
	$(PERL) ../tools/cutdeps makefile.bak > makefile
	gcc -MM $(CFLAGS) *.c >> makefile
	rm -f makefile.bak

