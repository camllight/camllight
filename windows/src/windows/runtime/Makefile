# Makefile for the Caml Light runtime system

SOURCEDIR=..\..\..\src
RUNTIMEDIR=$(SOURCEDIR)\runtime
TOOLSDIR=$(SOURCEDIR)\tools
GUIDIR=..\gui
CC=cl
INCLUDES=-I$(RUNTIMEDIR) -I$(SOURCEDIR) -I$(GUIDIR)
CFLAGS=$(INCLUDES) -DHAS_UI -Ox -G3 -MT -Zi
PERL=C:\bin\perl.exe

OBJS=interp.obj misc.obj stacks.obj fix_code.obj main.obj fail.obj signals.obj debugcom.obj \
  freelist.obj major_gc.obj minor_gc.obj memory.obj alloc.obj roots.obj \
  compare.obj ints.obj floats.obj str.obj io.obj \
  extern.obj externcp.obj intern.obj interncp.obj \
  hash.obj sys.obj meta.obj parsing.obj lexing.obj gc_ctrl.obj \
  msdos.obj graph.obj prims.obj nat.obj $(RUNTIMEDIR)\bignum\libbignum.lib

PRIMS=$(RUNTIMEDIR)\compare.c $(RUNTIMEDIR)\extern.c $(RUNTIMEDIR)\externcp.c $(RUNTIMEDIR)\floats.c $(RUNTIMEDIR)\gc_ctrl.c $(RUNTIMEDIR)\hash.c \
  $(RUNTIMEDIR)\intern.c $(RUNTIMEDIR)\interncp.c $(RUNTIMEDIR)\interp.c $(RUNTIMEDIR)\ints.c $(RUNTIMEDIR)\io.c $(RUNTIMEDIR)\lexing.c $(RUNTIMEDIR)\meta.c $(RUNTIMEDIR)\parsing.c \
  $(RUNTIMEDIR)\str.c $(RUNTIMEDIR)\sys.c $(RUNTIMEDIR)\graph.c	$(RUNTIMEDIR)\nat.c

GENSOURCES=prims prims.c opnames.h jumptbl.h

all: libcaml.lib

libcaml.lib: $(OBJS)
	lib /out:libcaml.lib $(OBJS)

prims: $(PRIMS)
    $(PERL) $(TOOLSDIR)/findprim prims <<
$(PRIMS)
<<

prims.c: prims
	$(PERL) $(TOOLSDIR)/mkprims prims > prims.c

opnames.h : $(RUNTIMEDIR)\instruct.h
	$(PERL) $(TOOLSDIR)/mkopnam $(RUNTIMEDIR)/instruct.h > opnames.h

# jumptbl.h is required only if you have GCC 2.0 or later
jumptbl.h : $(RUNTIMEDIR)\instruct.h
	$(PERL) $(TOOLSDIR)/mkjumptbl $(RUNTIMEDIR)/instruct.h > jumptbl.h

clean:
	rm -f $(GENSOURCES)
	rm -f *.obj *.lib

{$(RUNTIMEDIR)}.c{}.obj:
	$(CC) $(CFLAGS) -c $<


depend: $(GENSOURCES)
	mv makefile makefile.bak
	$(PERL) $(TOOLSDIR)/cutdeps makefile.bak > makefile
	gcc -MM $(INCLUDES) -DHAS_UI *.c $(RUNTIMEDIR)\*.c >> makefile
	rm -f makefile.bak


### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
prims.obj: prims.c ..\..\..\src\runtime/mlvalues.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/prims.h
alloc.obj: ..\..\..\src\runtime\alloc.c \
  ..\..\..\src\runtime/alloc.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/debugger.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/memory.h ..\..\..\src\runtime/gc.h \
  ..\..\..\src\runtime/minor_gc.h ..\..\..\src\runtime/stacks.h
compare.obj: ..\..\..\src\runtime\compare.c \
  ..\..\..\src\runtime/fail.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/memory.h \
  ..\..\..\src\runtime/gc.h ..\..\..\src\runtime/major_gc.h \
  ..\..\..\src\runtime/freelist.h ..\..\..\src\runtime/minor_gc.h \
  ..\..\..\src\runtime/str.h
debugcom.obj: ..\..\..\src\runtime\debugcom.c \
  ..\..\..\src\runtime/misc.h ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/debugger.h ..\..\..\src\runtime/mlvalues.h \
  ..\..\..\src\runtime/fail.h ..\..\..\src\runtime/stacks.h \
  ..\..\..\src\runtime/memory.h ..\..\..\src\runtime/gc.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/minor_gc.h ..\..\..\src\runtime/io.h \
  ..\..\..\src\runtime/debugcom.h
debugger.obj: ..\..\..\src\runtime\debugger.c
dummy.obj: ..\..\..\src\runtime\dummy.c \
  ..\..\..\src\runtime/fail.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h
extern.obj: ..\..\..\src\runtime\extern.c \
  ..\..\..\src\runtime/debugger.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/fail.h \
  ..\..\..\src\runtime/gc.h ..\..\..\src\runtime/intext.h \
  ..\..\..\src\runtime/io.h ..\..\..\src\runtime/memory.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/minor_gc.h
externcp.obj: ..\..\..\src\runtime\externcp.c \
  ..\..\..\src\runtime/debugger.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/fail.h \
  ..\..\..\src\runtime/gc.h ..\..\..\src\runtime/intext.h \
  ..\..\..\src\runtime/io.h ..\..\..\src\runtime/memory.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/minor_gc.h ..\..\..\src\runtime/reverse.h
fail.obj: ..\..\..\src\runtime\fail.c \
  ..\..\..\src\runtime/alloc.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/fail.h \
  ..\..\..\src\runtime/memory.h ..\..\..\src\runtime/gc.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/minor_gc.h ..\..\..\src\runtime/signals.h
fix_code.obj: ..\..\..\src\runtime\fix_code.c \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/instruct.h \
  ..\..\..\src\runtime/reverse.h
floats.obj: ..\..\..\src\runtime\floats.c \
  ..\..\..\src\runtime/alloc.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/debugger.h \
  ..\..\..\src\runtime/fail.h ..\..\..\src\runtime/memory.h \
  ..\..\..\src\runtime/gc.h ..\..\..\src\runtime/major_gc.h \
  ..\..\..\src\runtime/freelist.h ..\..\..\src\runtime/minor_gc.h
freelist.obj: ..\..\..\src\runtime\freelist.c \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/debugger.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/gc.h ..\..\..\src\runtime/gc_ctrl.h \
  ..\..\..\src\runtime/major_gc.h
gc_ctrl.obj: ..\..\..\src\runtime\gc_ctrl.c \
  ..\..\..\src\runtime/alloc.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/debugger.h \
  ..\..\..\src\runtime/gc.h ..\..\..\src\runtime/gc_ctrl.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/minor_gc.h
graph.obj: ..\..\..\src\runtime\graph.c \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/fail.h
hash.obj: ..\..\..\src\runtime\hash.c \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/memory.h ..\..\..\src\runtime/gc.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/minor_gc.h ..\..\..\src\runtime/str.h
intern.obj: ..\..\..\src\runtime\intern.c \
  ..\..\..\src\runtime/debugger.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/fail.h \
  ..\..\..\src\runtime/gc.h ..\..\..\src\runtime/intext.h \
  ..\..\..\src\runtime/io.h ..\..\..\src\runtime/memory.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/minor_gc.h ..\..\..\src\runtime/reverse.h
interncp.obj: ..\..\..\src\runtime\interncp.c \
  ..\..\..\src\runtime/debugger.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/fail.h \
  ..\..\..\src\runtime/gc.h ..\..\..\src\runtime/intext.h \
  ..\..\..\src\runtime/io.h ..\..\..\src\runtime/memory.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/minor_gc.h ..\..\..\src\runtime/reverse.h
interp.obj: ..\..\..\src\runtime\interp.c \
  ..\..\..\src\runtime/alloc.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/debugger.h \
  ..\..\..\src\runtime/fail.h ..\..\..\src\runtime/instruct.h \
  ..\..\..\src\runtime/memory.h ..\..\..\src\runtime/gc.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/minor_gc.h ..\..\..\src\runtime/prims.h \
  ..\..\..\src\runtime/signals.h ..\..\..\src\runtime/stacks.h \
  ..\..\..\src\runtime/str.h ..\..\..\src\runtime/unalignd.h \
  ..\..\..\src\runtime/debugcom.h ..\gui/ui.h jumptbl.h
ints.obj: ..\..\..\src\runtime\ints.c \
  ..\..\..\src\runtime/alloc.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/fail.h \
  ..\..\..\src\runtime/memory.h ..\..\..\src\runtime/gc.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/minor_gc.h
io.obj: ..\..\..\src\runtime\io.c \
  ..\..\..\src\runtime/alloc.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/debugger.h \
  ..\..\..\src\runtime/fail.h ..\..\..\src\runtime/io.h \
  ..\..\..\src\runtime/memory.h ..\..\..\src\runtime/gc.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/minor_gc.h ..\..\..\src\runtime/signals.h \
  ..\..\..\src\runtime/sys.h ..\gui/ui.h
lexing.obj: ..\..\..\src\runtime\lexing.c \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/stacks.h ..\..\..\src\runtime/memory.h \
  ..\..\..\src\runtime/gc.h ..\..\..\src\runtime/major_gc.h \
  ..\..\..\src\runtime/freelist.h ..\..\..\src\runtime/minor_gc.h \
  ..\..\..\src\runtime/str.h
main.obj: ..\..\..\src\runtime\main.c \
  ..\..\..\src\runtime/alloc.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/debugcom.h \
  ..\..\..\src\runtime/debugger.h ..\..\..\src\runtime/exec.h \
  ..\..\..\src\runtime/fail.h ..\..\..\src\runtime/gc_ctrl.h \
  ..\..\..\src\runtime/globals.h ..\..\..\src\runtime/intext.h \
  ..\..\..\src\runtime/io.h ..\..\..\src\runtime/stacks.h \
  ..\..\..\src\runtime/memory.h ..\..\..\src\runtime/gc.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/minor_gc.h ..\..\..\src\runtime/sys.h \
  ..\..\..\src/version.h
major_gc.obj: ..\..\..\src\runtime\major_gc.c \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/debugger.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/fail.h \
  ..\..\..\src\runtime/freelist.h ..\..\..\src\runtime/gc.h \
  ..\..\..\src\runtime/gc_ctrl.h ..\..\..\src\runtime/globals.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/roots.h
memory.obj: ..\..\..\src\runtime\memory.c \
  ..\..\..\src\runtime/debugger.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/fail.h \
  ..\..\..\src\runtime/freelist.h ..\..\..\src\runtime/gc.h \
  ..\..\..\src\runtime/gc_ctrl.h ..\..\..\src\runtime/major_gc.h \
  ..\..\..\src\runtime/memory.h ..\..\..\src\runtime/minor_gc.h
meta.obj: ..\..\..\src\runtime\meta.c \
  ..\..\..\src\runtime/alloc.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/globals.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/memory.h ..\..\..\src\runtime/gc.h \
  ..\..\..\src\runtime/minor_gc.h ..\..\..\src\runtime/prims.h
minor_gc.obj: ..\..\..\src\runtime\minor_gc.c \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/debugger.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/fail.h \
  ..\..\..\src\runtime/gc.h ..\..\..\src\runtime/gc_ctrl.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/memory.h ..\..\..\src\runtime/minor_gc.h \
  ..\..\..\src\runtime/roots.h
misc.obj: ..\..\..\src\runtime\misc.c \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/debugger.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/mlvalues.h ..\gui/ui.h
msdos.obj: ..\..\..\src\runtime\msdos.c \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h ..\..\..\src\runtime/misc.h
parsing.obj: ..\..\..\src\runtime\parsing.c \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/memory.h ..\..\..\src\runtime/gc.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/minor_gc.h ..\..\..\src\runtime/alloc.h
prims.obj: ..\..\..\src\runtime\prims.c \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/prims.h
roots.obj: ..\..\..\src\runtime\roots.c \
  ..\..\..\src\runtime/debugger.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/memory.h \
  ..\..\..\src\runtime/gc.h ..\..\..\src\runtime/major_gc.h \
  ..\..\..\src\runtime/freelist.h ..\..\..\src\runtime/minor_gc.h \
  ..\..\..\src\runtime/stacks.h
signals.obj: ..\..\..\src\runtime\signals.c \
  ..\..\..\src\runtime/alloc.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/debugger.h \
  ..\..\..\src\runtime/signals.h ..\..\..\src\runtime/stacks.h \
  ..\..\..\src\runtime/memory.h ..\..\..\src\runtime/gc.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/minor_gc.h
stacks.obj: ..\..\..\src\runtime\stacks.c \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/debugger.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/debugcom.h \
  ..\..\..\src\runtime/fail.h ..\..\..\src\runtime/stacks.h \
  ..\..\..\src\runtime/memory.h ..\..\..\src\runtime/gc.h \
  ..\..\..\src\runtime/major_gc.h ..\..\..\src\runtime/freelist.h \
  ..\..\..\src\runtime/minor_gc.h
str.obj: ..\..\..\src\runtime\str.c \
  ..\..\..\src\runtime/alloc.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/debugger.h
sys.obj: ..\..\..\src\runtime\sys.c \
  ..\..\..\src\runtime/alloc.h ..\..\..\src\runtime/misc.h \
  ..\..\..\src\runtime/config.h \
  ..\..\..\src\runtime\..\..\config/m.h \
  ..\..\..\src\runtime\..\..\config/s.h \
  ..\..\..\src\runtime/mlvalues.h ..\..\..\src\runtime/debugcom.h \
  ..\..\..\src\runtime/fail.h ..\..\..\src\runtime/globals.h \
  ..\..\..\src\runtime/instruct.h ..\..\..\src\runtime/signals.h \
  ..\..\..\src\runtime/stacks.h ..\..\..\src\runtime/memory.h \
  ..\..\..\src\runtime/gc.h ..\..\..\src\runtime/major_gc.h \
  ..\..\..\src\runtime/freelist.h ..\..\..\src\runtime/minor_gc.h \
  ..\gui/ui.h
