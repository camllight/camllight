TOPDIR=C:\caml
BINDIR=$(TOPDIR)\bin
LIBDIR=$(TOPDIR)\lib
EXDIR =$(TOPDIR)\examples
EXDIR_SLASH=$(EXDIR:\=/)
INFODIR =$(TOPDIR)\info
DOCDIR=$(TOPDIR)\doc

TXTFILES = copyrght.txt readme.txt install.txt lisezmoi.txt

DJGPPDIR=C:/djgpp/bin
DJGPPFILES=$(DJGPPDIR)/cwsdpmi.exe $(DJGPPDIR)/emu387.dxe
	
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
	cp windows\gui\WinRelease\Camlwin.exe $(TOPDIR)\Camlwin.exe
	cd ..
	cp camlwin.ini $(TXTFILES) $(TOPDIR)
	xcopy /e /s /i examples $(EXDIR)
	rm -rf $(EXDIR_SLASH)/CVS $(EXDIR_SLASH)/*/CVS
	xcopy /e /s /i info $(INFODIR)
	rm -rf $(INFODIR)/CVS
	cp $(DJGPPFILES) $(BINDIR)

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
	cd windows
        nmake

