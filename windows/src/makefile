# BINDIR=\caml7\bin
# LIBDIR=\caml7\lib

TOPDIR=C:\CAML7
BINDIR=$(TOPDIR)\bin
LIBDIR=$(TOPDIR)\lib
EXDIR =$(TOPDIR)\examples
SRCDIR=$(TOPDIR)\src
DOCDIR =$(TOPDIR)\doc

GUIDIR	 = $(TOPDIR)\src\windows\gui
APPLI_DIR= $(GUIDIR)\WinRelease
TXTFILES = copyrght.txt readme.txt install.txt

	
all: world gui

# Build the system for the first time
world:
	cd tools
	nmake all 
	cd ..\runtime
	nmake all 
	copy camlrun.exe ..
	cd ..\launch
	nmake all
	cd ..\yacc
	nmake all
	copy camlyacc.exe ..
	cd ..\lib
	nmake all
	cd ..\compiler
	nmake all
	cd ..\linker
	nmake all
	cd ..\librar
	nmake all
	cd ..\lex
	nmake all
	cd ..\toplevel
	nmake all
	cd ..
	
# Rebuild the system (bootstrap)
bootstrap: promote again compare

# Make the newly compiled compiler the current compiler
promote:
	mv camlcomp camlcomp.bak
	copy compiler\camlcomp camlcomp
	mv camllink camllink.bak
	copy linker\camllink camllink
	mv camllibr camllibr.bak
	copy librar\camllibr camllibr
	mv camllex camllex.bak
	copy lex\camllex camllex

# Recompile all Caml code from scratch
again:
	cd lib
	nmake clean all
	cd ..\compiler
	nmake clean all
	cd ..\linker
	nmake clean all
	cd ..\librar
	nmake clean all
	cd ..\lex
	nmake clean all
	cd ..\toplevel
	nmake clean all
	cd ..

# Compare the current compiler with the newly compiled one
compare:
	- tools\cmpcode camlcomp compiler\camlcomp
	- tools\cmpcode camllink linker\camllink
	- tools\cmpcode camllibr librar\camllibr
	- tools\cmpcode camllex lex\camllex

# Install the Caml Light system
install:
	mkdir $(LIBDIR)
	mkdir $(BINDIR)
	cd runtime
	nmake BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd ..\launch
	nmake BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd ..\lib
	nmake BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd ..\compiler
	nmake BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd ..\linker
	nmake BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd ..\librar
	nmake BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd ..\toplevel
	nmake BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd ..\yacc
	nmake BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd ..\lex
	nmake BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) install
	cd ..
	cp camlmsgs.txt $(LIBDIR)

# Remove the Caml Light system after installation
uninstall:
	del $(BINDIR)\*.*
	del $(LIBDIR)\*.*

# Remove all generated files
clean:
	cd tools
	nmake clean
	cd ..\runtime
	nmake clean
	cd ..\launch
	nmake clean
	cd ..\lib
	nmake clean
	cd ..\compiler
	nmake clean
	cd ..\linker
	nmake clean
	cd ..\librar
	nmake clean
	cd ..\toplevel
	nmake clean
	cd ..\yacc
	nmake clean
	cd ..\lex
	nmake clean
	cd ..\windows
	nmake clean

# Rebuild the dependencies in all Makefiles
depend:
	cd runtime
	nmake depend
	cd ..\lib
	nmake depend
	cd ..\compiler
	nmake depend
	cd ..\linker
	nmake depend
	cd ..\librar
	nmake depend
	cd ..\toplevel
	nmake depend
	cd ..\yacc
	nmake depend
	cd ..\lex
	nmake depend
	cd ..

gui:
	cd $(GUIDIR)
        nmake -f camlwin.mak CFG="Camlwin - Win32 Release"

# Make the distribution:
distrib:
	cd $(TOPDIR)
	rm -rf CAML
	md CAML
	copy $(APPLI_DIR)\CAMLWIN.EXE CAML
	cp camlwin.ini CAML 
	cp $(TXTFILES) CAML
	xcopy /e /s /i $(BINDIR) CAML\BIN
	xcopy /e /s /i $(LIBDIR) CAML\LIB
	xcopy /e /s /i $(EXDIR) CAML\EXAMPLES
	xcopy /e /s /i $(DOCDIR) CAML\DOC
	rm -f cl71win.zip
    pkzip -r -P -ex cl71win.zip CAML\*.*
	rm -rf CAML

