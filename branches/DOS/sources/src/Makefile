BINDIR=C:\caml386\bin
LIBDIR=C:\caml386\lib

# Build the system for the first time
world:
	cd tools
	make all
	cd ..\runtime
	make all
	copy camlrun.exe ..
	cd ..\launch
	make all
	cd ..\yacc
	make all
	copy camlyacc.exe ..
	cd ..\lib
	make all
	cd ..\compiler
	make all
	cd ..\linker
	make all
	cd ..\librar
	make all
	cd ..\lex
	make all
	cd ..\toplevel
	make all
	cd ..
	
# Rebuild the system (bootstrap)
bootstrap: promote again compare

# Make the newly compiled compiler the current compiler
promote:
	del camlcomp.bak
	rename camlcomp camlcomp.bak
	copy compiler\camlcomp camlcomp
	del camllink.bak
	rename camllink camllink.bak
	copy linker\camllink camllink
	del camllibr.bak
	rename camllibr camllibr.bak
	copy librar\camllibr camllibr
	del camltop.bak
	rename camltop camltop.bak
	copy toplevel\camltop camltop
	del camllex.bak
	rename camllex camllex.bak
	copy lex\camllex camllex

# Recompile all Caml code from scratch
again:
	cd lib
	make clean all
	cd ..\compiler
	make clean all
	cd ..\linker
	make clean all
	cd ..\librar
	make clean all
	cd ..\lex
	make clean all
	cd ..\toplevel
	make clean all
	cd ..
	cd lib
	make clean all
	cd ..\compiler
	make clean all
	cd ..\linker
	make clean all
	cd ..\librar
	make clean all
	cd ..\lex
	make clean all
	cd ..\toplevel
	make clean all
	cd ..

# Compare the current compiler with the newly compiled one
compare:
	- tools\cmpcode camlcomp compiler\camlcomp
	- tools\cmpcode camllink linker\camllink
	- tools\cmpcode camllibr librar\camllibr
	- tools\cmpcode camltop toplevel\camltop
	- tools\cmpcode camllex lex\camllex

# Install the Caml Light system
install:
	mkdir $(LIBDIR)
	mkdir $(BINDIR)
	cd runtime
	make -DBINDIR=$(BINDIR) -DLIBDIR=$(LIBDIR) install
	cd ..\launch
	make -DBINDIR=$(BINDIR) -DLIBDIR=$(LIBDIR) install
	cd ..\lib
	make -DBINDIR=$(BINDIR) -DLIBDIR=$(LIBDIR) install
	cd ..\compiler
	make -DBINDIR=$(BINDIR) -DLIBDIR=$(LIBDIR) install
	cd ..\linker
	make -DBINDIR=$(BINDIR) -DLIBDIR=$(LIBDIR) install
	cd ..\librar
	make -DBINDIR=$(BINDIR) -DLIBDIR=$(LIBDIR) install
	cd ..\toplevel
	make -DBINDIR=$(BINDIR) -DLIBDIR=$(LIBDIR) install
	cd ..\yacc
	make -DBINDIR=$(BINDIR) -DLIBDIR=$(LIBDIR) install
	cd ..\lex
	make -DBINDIR=$(BINDIR) -DLIBDIR=$(LIBDIR) install
	cd ..

# Remove the Caml Light system after installation
uninstall:
	del $(BINDIR)\*.*
	del $(LIBDIR)\*.*

# Remove all generated files
clean:
	cd tools
	make clean
	cd ..\runtime
	make clean
	cd ..\launch
	make clean
	cd ..\lib
	make clean
	cd ..\compiler
	make clean
	cd ..\linker
	make clean
	cd ..\librar
	make clean
	cd ..\toplevel
	make clean
	cd ..\yacc
	make clean
	cd ..\lex
	make clean
	cd ..

# Rebuild the dependencies in all Makefiles
depend:
	cd runtime
	make depend
	cd ..\lib
	make depend
	cd ..\compiler
	make depend
	cd ..\linker
	make depend
	cd ..\librar
	make depend
	cd ..\toplevel
	make depend
	cd ..\lex
	make depend
	cd ..

